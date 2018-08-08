-module(diameter_worker).

-export([initial_state/0, metrics/0, connect/3, disconnect/2, call/4]).

-include_lib("diameter/include/diameter.hrl").

-record(st, {service :: atom(),
             service_name :: string(),
             application :: atom()
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
%%   you have to define this modules as resources with module names
%% - origin_host :: default to `inet:gethostname()'
%% - origin_realm :: default to "testdomain-client.com"
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
    {ok, Hostname} = inet:gethostname(),
    DictMod = to_atom(proplists:get_value(dictionary, Param)),
    check_module(DictMod, [{vendor_id, 0}, {dict, 0}]),
    CbMod = to_atom(proplists:get_value(module, Param, diameter_wroker_client_cb)),
    check_module(CbMod, [{peer_up,3}, {peer_down,3}, {pick_peer,4}, {prepare_request,3},
                         {prepare_retransmit,3}, {handle_answer,4}, {handle_error,4},
                         {handle_request,3}]),

    SvcOpts = [{'Origin-Host', proplists:get_value(origin_host, Param, Hostname)},
               {'Origin-Realm', proplists:get_value(origin_realm, Param, "testdomain-client.com")},
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
    receive
        #diameter_event{service = SvcName, info = Info}
          when element(1, Info) == up ->
            diameter:unsubscribe(SvcName),
            ok;
        _ ->
            diameter:unsubscribe(SvcName),
            error(diameter_client_not_started)
    end,
    mzb_metrics:declare_metrics(metrics(SvcNameStr)),
    {nil, #st{service = SvcName,  service_name = SvcNameStr, application = DictMod}}.

-spec disconnect(#st{}, meta()) -> {nil, #st{}}.
disconnect(St=#st{service = SvcName}, _Meta) ->
    diameter:stop_service(SvcName),
    {nil, St}.

-spec call(#st{}, meta(), string(), map()) -> {nil, #st{}}.
call(St=#st{service = SvcName, application = AppName}, _Meta, MsgName, Msg) ->
    Now = erlang:monotonic_time(microsecond),
    case diameter:call(SvcName, AppName, [to_atom(MsgName)|resmap_to_msg(Msg)], []) of
        {ok, [_MsgName|Msg]} ->
            mzb_metrics:notify({metric_name(St, "success"), counter}, 1);
        {error, timeout} ->
            mzb_metrics:notify({metric_name(St, "errors"), counter}, 1);
        {error, _} ->
            mzb_metrics:notify({metric_name(St, "errors"), counter}, 1)
    end,
    Duration = erlang:monotonic_time(microsecond) - Now,
    mzb_metrics:notify({metric_name(St, "latency"), histogram}, Duration),
    {nil, St}.

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
    Exports = module:module_info(exports),
    case Funs -- Exports of
        [] -> ok;
        Undef ->
            error({not_exported, {Mod, Undef}})
    end.

metric_name(#st{service_name = SN}, MN) ->
    SN ++ "." ++ MN.

comma_list(Str) ->
    string:split(Str, ",").

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
