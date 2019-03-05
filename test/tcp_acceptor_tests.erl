-module(tcp_acceptor_tests).
-include_lib("eunit/include/eunit.hrl").

start_and_stop_simple_test_() ->
    ok.
%    {ok, APid} = tcp_acceptor:start(),
%    ?assert(is_process_alive(APid))
%    {error,{already_registered,APid}} = tcp_acceptor:start().
%    ok = tcp_acceptor:stop().
%   false = is_process_alive(APid).
