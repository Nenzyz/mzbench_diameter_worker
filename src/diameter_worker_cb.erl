%%%-------------------------------------------------------------------
%%% @author Vladimir G. Sekissov <eryx67@gmail.com>
%%% @copyright (C) 2018, Vladimir G. Sekissov
%%% @doc
%%%
%%% @end
%%% Created : 18 Jul 2018 by Vladimir G. Sekissov <eryx67@gmail.com>
%%%-------------------------------------------------------------------
-module(diameter_worker_cb).

-export([peer_up/3, peer_down/3, pick_peer/4, prepare_request/3,
         prepare_retransmit/3, handle_answer/4, handle_error/4,
         handle_request/3]).

-include_lib("diameter/include/diameter.hrl").
-include_lib("diameter/include/diameter_gen_base_rfc6733.hrl").

-record(state, {}).

-type state() :: #state{}.
-type capabilities() :: #diameter_caps{}.
-type packet() ::  diameter_codec:packet().
-type message() ::  diameter_codec:message().
-type peer() :: {Peer_Ref :: term(), Capabilities :: capabilities()}.

%%----------------------------------------------------------------------
%%  The DIAMETER application callbacks
%%----------------------------------------------------------------------

-spec peer_up(SvcName, Peer, State) -> NewState
        when
                SvcName :: diameter:service_name(),
                Peer ::  peer(),
                State :: state(),
                NewState :: state().
%% @doc Invoked when the peer connection is available
peer_up(_SvcName, _Peer, State) ->
    State.

-spec peer_down(SvcName, Peer, State) -> NewState
        when
                SvcName :: diameter:service_name(),
                Peer :: peer(),
                State :: state(),
                NewState :: state().
%% @doc Invoked when the peer connection is not available
peer_down(_SvcName, _Peer, State) ->
    State.

-spec pick_peer(LocalCandidates, RemoteCandidates, SvcName, State) -> Result
        when
                LocalCandidates :: [peer()],
                RemoteCandidates :: [peer()],
                SvcName :: diameter:service_name(),
                State :: state(),
                NewState :: state(),
                Selection :: {ok, Peer} | {Peer, NewState},
                Peer :: peer() | false,
                Result :: Selection | false.
%% @doc Invoked as a consequence of a call to diameter:call/4 to select
%% a destination peer for an outgoing request.
pick_peer([Peer | _], _, _SvcName, _State) ->
        {ok, Peer}.

-spec prepare_request(Packet, SvcName, Peer) -> Action
        when
                Packet :: packet(),
                SvcName :: diameter:service_name(),
                Peer :: peer(),
                Action :: Send | Discard | {eval_packet, Action, PostF},
                Send :: {send, packet() | message()},
                Discard :: {discard, Reason} | discard,
                Reason :: term(),
                PostF :: diameter:evaluable().
%% @doc Invoked to return a request for encoding and transport
prepare_request(#diameter_packet{msg = Record}, _, {_, Caps}) ->
        #diameter_caps{origin_host = {OH, DH}, origin_realm = {OR, DR}} = Caps,
        Request = generate_request(Record, OH, DH, OR, DR),
        {send, Request}.

-spec prepare_retransmit(Packet, SvcName, Peer) -> Action
        when
                Packet :: packet(),
                SvcName :: diameter:service_name(),
                Peer :: peer(),
                Action :: Send | Discard | {eval_packet, Action, PostF},
                Send :: {send, packet() | message()},
                Discard :: {discard, Reason} | discard,
                Reason :: term(),
                PostF :: diameter:evaluable().
%% @doc Invoked to return a request for encoding and retransmission.
%% In case of peer connection is lost alternate peer is selected.
prepare_retransmit(Packet, SvcName, Peer) ->
        prepare_request(Packet, SvcName, Peer).

-spec handle_answer(Packet, Request, SvcName, Peer) -> Result
        when
                Packet :: packet(),
                Request :: message(),
                SvcName :: diameter:service_name(),
                Peer :: peer(),
                Result :: term().
%% @doc Invoked when an answer message is received from a peer.
handle_answer(#diameter_packet{msg =  Msg}, _Request, _SvcName, _Peer) ->
        {ok, Msg}.

-spec handle_error(Reason, Request, SvcName, Peer) -> Result
        when
                Reason :: timeout | failover | term(),
                Request :: message(),
                SvcName :: diameter:service_name(),
                Peer :: peer(),
                Result :: term().
%% @doc Invoked when an error occurs before an answer message is received
%% in response to an outgoing request.
handle_error(Reason, _Request, _SvcName, _Peer) ->
        {error, Reason}.

-spec handle_request(Packet, SvcName, Peer) -> Action
        when
                Packet :: packet(),
                SvcName :: term(),
                Peer :: peer(),
                Action :: Reply | {relay, [Opt]} | discard
                        | {eval|eval_packet, Action, PostF},
                Reply :: {reply, packet() | message()}
                        | {answer_message, 3000..3999|5000..5999}
                        | {protocol_error, 3000..3999},
                Opt :: diameter:call_opt(),
                PostF :: diameter:evaluable().
%% @doc Invoked when a request messge is received from the peer.
handle_request(#diameter_packet{msg = _Request, errors = _Errs}, _SvcName, {_Peer, _Caps}) ->
    {answer_message, ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY'}.

%%---------------------------------------------------------------------
%% Internal functions
%%---------------------------------------------------------------------

%% @hidden
generate_request([Name|AVPs], OHost, _DHost, ORealm, DRealm) ->
        [Name|AVPs#{'Origin-Host' => OHost,
                    'Origin-Realm' => ORealm,
                    'Destination-Realm' => DRealm
                   }].
