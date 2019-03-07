-module(tcp_acceptor).

% export public API
-export([start/0, start/1]).
-export([stop/0, stop/1]).

% =================== specs, records, constants, defaults =======================

% options accepted by start/1
-type options() :: #{
    'REG_NAME'                  => atom(),
    'START_ATTEMPTS'            => pos_integer(),
    'START_TIMEOUT_PER_ATTEMPT' => non_neg_integer(),
    'SHUTDOWN_TIMEOUT'          => non_neg_integer(),
    'PORT'                      => non_neg_integer(),
    'LISTEN_OPTIONS'            => [gen_tcp:listen_option()],
    'HANDLER'                   => fun(),
    'INIT_STATE'                => conn_state()
}.

% defaults (in case of some options not set)
-define(DEFAULT_OPTIONS,
    [
        % registered name of socket-listener server
        {'REG_NAME', ?MODULE},
        % max start attemts
        {'START_ATTEMPTS', 5},
        % max timeout for each start attempt
        {'START_TIMEOUT_PER_ATTEMPT', 10},
        % port for listening
        {'PORT', 12000},
        % listen options "on init"
        {'LISTEN_OPTIONS',  ['binary', {active, false}, {packet, 4}, {reuseaddr, true}]},
        % function to handle frames and manipulate current receiver state
        {'HANDLER', fun dump_frame/3},
        % init value of connection state
        {'INIT_STATE', <<>>}
    ]
).

-define(SHUTDOWN_TIMEOUT, 1000).

-record(state,  {
        l_socket        :: 'undefined' | gen_tcp:socket(),
        options         :: options(),
        ready = false   :: boolean()
    }).
-type state()           :: #state{}.

-type error()           :: {'error', reason()}.
-type reason()          :: term().
-type conn_state()      :: term().

% ------------- end of specs, records, constants, defaults ----------------------


% =============================== public api part ===============================

% @doc
% Interface for start tcp_acceptor with default options
% Start is synchronous. We returning from start once first acceptor is ready.
% @end

-spec start() -> Result when
    Result  ::  {'ok', pid()} | error().

start() -> start(#{}).


% @doc
% Interface for start tcp_acceptor with custom options
% Start is synchronous. We returning from start once first acceptor is ready.
% @end

-spec start(Options) -> Result when
    Options ::  options(),
    Result  ::  {'ok', pid()} | error().

start(Options) ->
    CompletedOps = assign_defaults(Options),
    Pid = spawn_link(fun() -> process_flag(trap_exit, true), server_loop(#state{options = CompletedOps}) end),
    RegName = maps:get('REG_NAME', CompletedOps),
    try register(RegName, Pid) of
        true ->
            case init(Pid, CompletedOps) of
                true ->
                    {'ok', Pid};
                false ->
                    _ = stop(Pid),
                    {error, 'acceptor_not_responding'}
            end
        catch error:_ ->
            {error, {'already_registered', whereis(RegName)}}
    end.

% @doc
% API for shutdown tcp_acceptor gracefully.
% Stop is synchronous. We returning from stop once get confirmation that tcp_acceptor is down.
% @end

% todo: implement force-kill routine in case of we do not get shutdown message in some amount of time
-spec stop() -> Result when
    Result  ::  'ok' | error().

stop() -> stop(?MODULE).

-spec stop(PidOrRegisterdName) -> Result when
    PidOrRegisterdName  :: pid() | atom(),
    Result              :: 'ok' | error().

stop('undefined') -> ok;
stop(RegisteredName) when is_atom(RegisteredName) ->
    stop(whereis(RegisteredName));
stop(Pid) ->
    unlink(Pid),
    MRef = erlang:monitor(process, Pid),
    exit(Pid, shutdown),
    receive
    	{'DOWN', MRef, process, Pid, _Reason} ->
    	    'ok';
        {'EXIT', Pid, shutdown} ->
    	    'ok';
        Other ->
            error_logger:warning_msg("Some unexpected message got at ~p(~p): ~p",[?MODULE, ?LINE, Other]),
            {'error', {'unexpected_message', Other}}
    after ?SHUTDOWN_TIMEOUT ->
        {'error', timeout}
    end.

% ------------------------- end of public api part ------------------------------

% ================================ internals ====================================

% server_loop. once we got shutdown message we have to close the socket
-spec server_loop(State) -> Result when
    State   :: state(),
    Result  :: no_return() | 'ok'.

server_loop(State) ->
    receive
        'init' ->
            Options = State#state.options,
            {ok, LSocket} = gen_tcp:listen(maps:get('PORT', Options), maps:get('LISTEN_OPTIONS', Options)),
            SocketOwner = self(),
            _ = spawn(fun() -> acceptor(LSocket, Options, SocketOwner) end),
            server_loop(State#state{l_socket = LSocket});
        'acceptor_ready' ->
            server_loop(State#state{ready = true});
        {'is_ready?', ReportTo} ->
            ReportTo ! {'ready', State#state.ready},
            server_loop(State);
        {'EXIT', _Pid, 'shutdown'} ->
            gen_tcp:close(State#state.l_socket);
        Other -> % ignoring other messages yet
            error_logger:warning_msg("Some unexpected message got at ~p(~p): ~p",[?MODULE, ?LINE, Other]),
            server_loop(State)
    end.


% init server
-spec init(Pid, Options) -> Result when
    Pid                 :: pid(),
    Options             :: options(),
    Result              :: boolean().

init(Pid, #{'START_TIMEOUT_PER_ATTEMPT' := Timeout, 'START_ATTEMPTS' := MAX_ATTEMPTS}) ->
    Pid ! 'init',
    is_ready(Pid, Timeout, Timeout * (MAX_ATTEMPTS + 1), MAX_ATTEMPTS, 0).


% check does acceptor is ready or not
-spec is_ready(Pid, TimeoutPerAttempt, TotalTimeout, MaxAttempts, CurrentAttempts) -> Result when
    Pid                 :: pid(),
    TimeoutPerAttempt   :: non_neg_integer(),
    TotalTimeout        :: non_neg_integer(),
    MaxAttempts         :: non_neg_integer(),
    CurrentAttempts     :: non_neg_integer(),
    Result              :: boolean().

is_ready(_Pid, _Timeout, _TotalTimeout, Attempts, Attempts) -> false;
is_ready(Pid, Timeout, TotalTimeout, MaxAttempts, Attempts) ->
    Pid ! {'is_ready?', self()},
    receive
        {'ready', true} ->
            true;
        {'ready', false} ->
            error_logger:info_msg("Waiting for acceptor"),
            timer:sleep(Timeout),
            is_ready(Pid, Timeout, TotalTimeout, MaxAttempts, Attempts + 1)
    after TotalTimeout ->
        false
    end.


% connection acceptor
-spec acceptor(LSocket, Options, ReportStartTo) -> Result when
    LSocket         :: gen_tcp:socket(),
    Options         :: options(),
    ReportStartTo   :: pid() | 'undefined',
    Result          :: no_return() | 'ok'.

acceptor(LSocket, Options, ReportStartTo) ->
    _ = may_report(ReportStartTo),
    case gen_tcp:accept(LSocket) of
        {'ok', ASocket} ->
            spawn(fun() -> acceptor(LSocket, Options, 'undefined') end),
            acceptor_loop(ASocket, Options, maps:get('INIT_STATE', Options));
        _Other ->
            'ok'
    end.


% on the first init we going report to the server
% that acceptor spawned succesfully and ready to gen_tcp:accept
-spec may_report(PidOrFalse) -> Result when
    PidOrFalse  :: pid() | 'undefined',
    Result      :: atom().

may_report('undefined') -> 'ok';
may_report(Pid) -> Pid ! 'acceptor_ready'.

% accepting messages in loop
-spec acceptor_loop(Socket, Options, State) -> Result when
    Socket  :: gen_tcp:socket(),
    Options :: options(),
    State   :: conn_state(),
    Result  :: no_return() | 'ok'.

acceptor_loop(Socket, #{'HANDLER' := Handler} = Options, State) ->
    ok = inet:setopts(Socket, [{active, once}]),
    receive
        {'tcp', Socket, Data} ->
            acceptor_loop(
                Socket,
                Options,
                erlang:apply(Handler, [Socket, Data, State])
            );
        {'tcp_closed', Socket} ->
            ok
    end.


% default function for handle frames
-spec dump_frame(Socket, Frame, State) -> Result when
    Socket  :: gen_tcp:socket(),
    Frame   :: term(),
    State   :: conn_state(),
    Result  :: conn_state().

dump_frame(_Socket, Frame, State) ->
    io:format("~p",[
        string:tokens(binary_to_list(Frame),"\r\n")
    ]), State.


% helper function for assign default values to options
-spec assign_defaults(Options) -> Result when
    Options :: options(),
    Result  :: options().

assign_defaults(Options) ->
    lists:foldl(
        fun({Key, Value}, Acc) ->
            maps:put(Key, maps:get(Key, Acc, Value), Acc)
        end,
        Options,
        ?DEFAULT_OPTIONS
    ).

% ---------------------------- end of internals ---------------------------------

