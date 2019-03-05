-module(tcp_acceptor).

% export public API
-export([start/0]).
-export([stop/0]).

% export internal server-routine API
-export([init/1]).

% =================== specs, records, constants, defaults =======================

% defaults
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
        {'LISTEN_OPTIONS',  ['binary', {active, false}, {packet, 4}]},
        % function to handle frames and manipulate current receiver state
        {'HANDLER', fun dump_frame/2}
    ]
).

-define(SHUTDOWN_TIMEOUT, 1000).

-type options() :: #{
    'REG_NAME'                  => atom(),
    'START_ATTEMPTS'            => pos_integer(),
    'START_TIMEOUT_PER_ATTEMPT' => non_neg_integer(),
    'SHUTDOWN_TIMEOUT'          => non_neg_integer(),
    'PORT'                      => non_neg_integer(),
    'LISTEN_OPTIONS'            => [gen_tcp:listen_option()],
    'HANDLER'                   => fun()
}.

-record(state,  {
        l_socket        :: gen_tcp:socket(),
        options         :: options(),
        ready = false   :: boolean()
    }).
%-type state()       :: #state{}.

-type error()       :: {'error', reason()}.
-type reason()      :: term().

% ------------- end of specs, records, constants, defaults ----------------------


% =============================== public api part ===============================

% @doc
% We returning from here only once acceptor ready to accept new connections.
% @end
-spec start() -> {'ok', pid()} | error().

start() -> start(#{}).

start(Options) ->
    CompletedOps = assign_defaults(Options),
    RegName = maps:get('REG_NAME', CompletedOps),
    case whereis(RegName) of
        'undefined' ->
            Pid = spawn_link(?MODULE, init, [CompletedOps]),
            case is_ready(
                    Pid,
                    maps:get('START_TIMEOUT_PER_ATTEMPT', CompletedOps),
                    maps:get('START_TIMEOUT_PER_ATTEMPT', CompletedOps)*(maps:get('START_ATTEMPTS', CompletedOps) + 1),
                    maps:get('START_ATTEMPTS', CompletedOps),
                    0
                ) of
                true ->
                    register(RegName, Pid),
                    {'ok', Pid};
                false ->
                    stop(Pid),
                    {error, 'acceptor_not_responding'}
            end;
        Pid ->
            {error, {'already_registered', Pid}}
    end.

assign_defaults(Options) ->
    lists:foldl(
        fun({Key, Value}, Acc) ->
            maps:put(Key, maps:get(Key, Acc, Value), Acc)
        end,
        Options,
        ?DEFAULT_OPTIONS
    ).


% @doc check does any acceptor is ready
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


% @doc
% Synchronous API for shutdown listener-process gracefully.
% todo: implement force-kill in case of we do not get shutdown message in some amount of time
% @end
-spec stop() -> 'ok' | error().

stop() -> stop(?MODULE).

stop('undefined') -> ok;
stop(RegisteredName) when is_atom(RegisteredName) ->
    stop(whereis(?MODULE));
stop(Pid) ->
    erlang:monitor(process, Pid),
    exit(Pid, shutdown),
    receive
    	{'DOWN', _MRef, process, Pid, _Reason} ->
    	    'ok'
    after ?SHUTDOWN_TIMEOUT ->
        {'error', timeout}
    end.

% =============================== public api part ===============================


% ------------------------- end of public api part ------------------------------

% init section of server
init(#{'PORT' := Port, 'LISTEN_OPTIONS' := ListenOptions} = Options) ->
    {ok, LSocket} = gen_tcp:listen(Port, ListenOptions),
    SocketOwner = self(),
    spawn(fun() -> acceptor(LSocket, Options, SocketOwner) end),
    server_loop(#state{l_socket = LSocket, options = Options}).

% server_loop. once we got shutdown message we have to close the socket
server_loop(State) ->
    receive
        'acceptor_ready' ->
            server_loop(State#state{ready = true});
        {'is_ready?', ReportTo} ->
            ReportTo ! {'ready', State#state.ready},
            server_loop(State);
        {'EXIT', _Pid, 'shutdown'} ->
            gen_tcp:close(State#state.l_socket);
        _Other -> % ignoring other messages yet
            server_loop(State)
    end.

% connection acceptor
acceptor(LSocket, Options, ReportStartTo) ->
    may_report(ReportStartTo),
    case gen_tcp:accept(LSocket) of
        {'ok', ASocket} ->
            spawn(fun() -> acceptor(LSocket, Options, false) end),
            acceptor_loop(ASocket, Options, 'undefined');
        _Other ->
            ok
    end.

% on the first init we going report to the server
% that acceptor spawned succesfully and ready to gen_tcp:accept
-spec may_report(PidOrFalse) -> Result when
    PidOrFalse  :: pid() | false,
    Result      :: ok.

may_report(false) -> ok;
may_report(Pid) -> Pid ! 'acceptor_ready'.


% accepting messages in loop
acceptor_loop(Socket, #{'HANDLER' := Handler} = Options, State) ->
    inet:setopts(Socket, [{active, once}]),
    receive
        {'tcp', Socket, Data} ->
            acceptor_loop(
                Socket,
                Options,
                erlang:apply(Handler, [Data, State])
            );
        {'tcp_closed', Socket} ->
            ok
    end.

% default function for handle frames
dump_frame(Frame, _State) ->
    io:format("~p",[
        string:tokens(binary_to_list(Frame),"\r\n")
    ]).

% ---------------------------- end of internals ---------------------------------

