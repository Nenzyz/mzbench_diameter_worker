-module(diameter_worker).

-export([initial_state/0, metrics/0, connect/3, disconnect/2, call/3, call/4]).

-include_lib("diameter/include/diameter.hrl").

-record(st, {service :: atom(),
             service_name :: string(),
             application :: atom(),
             required_avps :: map()
            }).

-type meta() :: [{Key :: atom(), Value :: any()}].

-type graph_group() :: {group, Name :: string(), [graph()]}
                     | graph().
-type graph()       :: {graph, Opts :: #{metrics => [metric()],
                                         units => string(),
                                         title => string()}}
                     | [metric()]
                     | metric().
-type metric()      :: {Name :: string(), Type :: metric_type() }
                     | {Name :: string(), Type :: metric_type(), Opts :: map() }.
-type metric_type() :: counter | gauge | histogram.


-spec initial_state() -> #st{}.
initial_state() -> #st{}.

-spec metrics() -> [graph_group()].
metrics() -> [].

metrics(St=#st{service_name = SN}) ->
    [
     {group, "DIAMETER(" ++ SN ++ ")",
      [
       {graph, #{title => "Messages",
                 units => "N",
                 metrics => [{metric_name(St, "success"), counter},
                             {metric_name(St, "errors"), counter},
                             {metric_name(St, "timeout"), counter}
                            ]}},
       {graph, #{title => "Latency",
                 units => "microseconds",
                 metrics => [{metric_name(St, "latency"), histogram}]}}
      ]}
    ].

-type connect_opt() :: {module, string()}
                     | {dictionary, string()}
                     | {modules, string()}
                     | {origin_host, string()}
                     | {origin_realm, string()}
                     | {product_name, string()}
                     | {auth_application_id, string()}
                     | {acct_application_id, string()}
                     | {service_name, string()}
                     | {address, string()}
                     | {port, integer()}
                     | {avp_dictionaries, string()}
                     | {transport, string()}
                     | {capability, #{'Origin-State-Id' => non_neg_integer(),
                                      'Supported-Vendor-Id' => [non_neg_integer()],
                                      'Inband-Security-Id' => [non_neg_integer()],
                                      'Vendor-Specific-Application-Id' => [map()],
                                      'Firmware-Revision' => non_neg_integer()
                                     }}.

%% @doc Connect to diameter server.
%% Options:
%% - dictionary :: diameter dictionary module name
%% - module :: callback module default to `diameter_worker_cb'
%%   must implement `diameter_app' behavior
%% - origin_host :: default to `inet:gethostname()'
%% - origin_realm :: default to "mzbench-client.com"
%% - product_name :: default to "MzBench"
%% - auth_application_id :: comma separated list of integers, default to empty string
%% - acct_application_id :: comma separated list of integers, default to empty string
%% - service_name :: distinct one diameter service from another, must be unique per client,
%%   default to `diameter_worker'
%% - address :: server address, default to "127.0.0.1"
%% - port :: server port, default to 3868
%% - avp_dictionaries :: comma separated list of diameter module dictionaries
%%   default to empty string
%% - transport :: "tcp" or "sctp", default to "tcp"
%% - capability :: #{'Origin-State-Id' => non_neg_integer(),
%%                   'Supported-Vendor-Id' => [non_neg_integer()],
%%                   'Inband-Security-Id' => [non_neg_integer()],
%%                   'Vendor-Specific-Application-Id' => [map()],
%%                   'Firmware-Revision' => non_neg_integer()
%%                  }
%% @end
-spec connect(#st{}, meta(), [connect_opt()]) -> {nil, #st{}}.
connect(_State, _Meta, Param) ->
    {ok, _} = application:ensure_all_started(diameter),
    {ok, Hostname} = inet:gethostname(),
    DictMod = to_atom(proplists:get_value(dictionary, Param)),
    check_module(DictMod, [{vendor_id, 0}, {dict, 0}]),
    CbMod = to_atom(proplists:get_value(module, Param, diameter_worker_cb)),
    check_module(CbMod, [{peer_up,3}, {peer_down,3}, {pick_peer,4}, {prepare_request,3},
                         {prepare_retransmit,3}, {handle_answer,4}, {handle_error,4},
                         {handle_request,3}]),
    OriginHost = proplists:get_value(origin_host, Param, Hostname),
    OriginRealm = proplists:get_value(origin_realm, Param, "mzbench-client.com"),
    SvcOpts = [{'Origin-Host', OriginHost},
               {'Origin-Realm', OriginRealm},
               {'Vendor-Id', DictMod:vendor_id()},
               {'Product-Name', proplists:get_value(product_name, Param, "MzBench")},
               {'Auth-Application-Id', int_list(proplists:get_value(auth_application_id, Param, ""))},
               {'Acct-Application-Id', int_list(proplists:get_value(acct_application_id, Param, ""))},
               {string_decode, false},
               {restrict_connections, false},
               {decode_format, map},
               {application, [{dictionary, DictMod},
                              {module, CbMod}
                             ]}
              ] ++
        maps:to_list(resmap_to_msg(proplists:get_value(capability, Param, #{}))),
    SvcNameStr = proplists:get_value(service_name, Param, ?MODULE_STRING),
    SvcName = to_atom(SvcNameStr),
    Address = proplists:get_value(address, Param, "127.0.0.1"),
    Port = proplists:get_value(port, Param, 3868),
    AvpDicts = atom_list(proplists:get_value(avp_dictionaries, Param, [])),
    TrMod = transport_module(proplists:get_value(transport, Param, tcp)),
    TrOpts = [{connect_timer, 30000}, {transport_module, TrMod},
              {avp_dictionaries, AvpDicts},
              {transport_config, maps:to_list(#{raddr => Address,
                                                rport => Port,
                                                reuseaddr => true
                                               })}
             ],

    ok = diameter:start_service(SvcName, SvcOpts),
    true = diameter:subscribe(SvcName),
    {ok, _Ref} = diameter:add_transport(SvcName, {connect, TrOpts}),
    #diameter_caps{origin_host = {OH, _DH}, origin_realm = {OR, DR}} =
        receive
            #diameter_event{service = SvcName, info = {up, _Ref, {_, Caps}, _C}} ->
                diameter:unsubscribe(SvcName),
                Caps;
            #diameter_event{service = SvcName, info = {up, _Ref, {_, Caps}, _C, _P}} ->
                diameter:unsubscribe(SvcName),
                Caps;
            _ ->
                diameter:unsubscribe(SvcName),
                error(diameter_client_not_started)
        end,
    St = #st{service = SvcName,  service_name = SvcNameStr, application = DictMod,
             required_avps = #{'Session-Id' => diameter:session_id(OriginHost),
                               'Origin-State-Id' => [diameter:origin_state_id()],
                               'Origin-Host' => OH,
                               'Origin-Realm' => OR,
                               'Destination-Realm' => DR
                              }
            },
    mzb_metrics:declare_metrics(metrics(St)),
    {nil, St}.

-spec disconnect(#st{}, meta()) -> {nil, #st{}}.
disconnect(St=#st{service = SvcName}, _Meta) ->
    diameter:stop_service(SvcName),
    {nil, St}.

call(InitSt, Meta, Msgs) when is_list(Msgs) ->
    lists:mapfoldl(fun (#{message_name := MsgName, message := Msg}, St) ->
                           call(St, Meta, MsgName, Msg)
                   end, InitSt, Msgs).

-spec call(#st{}, meta(), string(), map()) -> {any(), #st{}}.
call(St=#st{service = SvcName, application = AppName, required_avps = ReqAvps}, _Meta, MsgName, Msg) ->
    Now = erlang:monotonic_time(microsecond),
    Msg1 = maps:merge(ReqAvps, resmap_to_msg(Msg)),
    Req = [to_atom(MsgName)|Msg1],
    Res = diameter:call(SvcName, AppName, Req, []),
    case Res of
        {ok, [_MsgName|_]} ->
            mzb_metrics:notify({metric_name(St, "success"), counter}, 1);
        {error, encode} ->
            error({encode, Req});
        {error, timeout} ->
            mzb_metrics:notify({metric_name(St, "errors"), counter}, 1);
        {error, _} ->
            mzb_metrics:notify({metric_name(St, "errors"), counter}, 1)
    end,
    Duration = erlang:monotonic_time(microsecond) - Now,
    mzb_metrics:notify({metric_name(St, "latency"), histogram}, Duration),
    {Res, St}.

%%==============================================================================
%% Utility functions
%%==============================================================================
resmap_to_msg(Map) when is_map(Map) ->
    maps:fold(fun (K, V, Acc) ->
                      AK = to_atom(K),
                      Acc#{AK => resmap_to_msg(V)}
              end, #{}, Map);
resmap_to_msg(ListMap=[Map|_]) when is_map(Map) ->
    lists:map(fun resmap_to_msg/1, ListMap);
resmap_to_msg(V) ->
    V.

transport_module(tcp) ->
    diameter_tcp;
transport_module(sctp) ->
    diameter_sctp;
transport_module(Name) ->
    error({invalid_transport, Name}).

check_module(Mod, Exports) ->
    ok = load_module_(Mod),
    ok = check_module_exports(Mod, Exports).

load_module_(Mod) ->
    case code:ensure_loaded(Mod) of
        {module, _} ->
            ok;
        {error, not_purged} ->
            ok;
        {error, sticky_directory} ->
            ok;
        {error, Err} ->
            error({Err, Mod})
    end.

check_module_exports(Mod, Funs) ->
    Exports = Mod:module_info(exports),
    case Funs -- Exports of
        [] -> ok;
        Undef ->
            error({not_exported, {Mod, Undef}})
    end.

metric_name(#st{service_name = SN}, MN) ->
    SN ++ "." ++ MN.

comma_list(Str) ->
    [V || V <- string:split(Str, ","), V /= ""].

atom_list(Str) ->
    lists:map(fun to_atom/1, comma_list(Str)).

int_list(Str) ->
    lists:map(fun erlang:list_to_integer/1, comma_list(Str)).

to_atom(V) when is_atom(V) ->
    V;
to_atom(V) when is_list(V) ->
    list_to_atom(V);
to_atom(V) when is_binary(V) ->
    erlang:binary_to_atom(V, utf8).
