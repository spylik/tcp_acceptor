-module(tcp_acceptor_tests).
-include_lib("eunit/include/eunit.hrl").

start_and_stop_without_connections_simple_test() ->
    {ok, APid} = tcp_acceptor:start(),
    ?assert(is_process_alive(APid)),
    {error,{already_registered, APid}} = tcp_acceptor:start(),
    ?assert(is_process_alive(APid)),
    ok = tcp_acceptor:stop(),
    ?assertNot(is_process_alive(APid)),
    ok = tcp_acceptor:stop().
