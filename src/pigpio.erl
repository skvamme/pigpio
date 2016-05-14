-module(pigpio).
-author(skvamme).
-export([start/0,init/0,loop/2]).
-define(PIGPIO_IP,"192.168.0.20").
-define(PIGPIO_PORT,8888).
%-compile(export_all).
-define(UINT,32/little).

start() -> Pid = spawn_link(?MODULE,init,[]), {ok,Pid}.

init() ->
        io:format("New process: ~p~n", [?MODULE]),
        inets:start(),
        {ok,Socket} = gen_tcp:connect(?PIGPIO_IP, ?PIGPIO_PORT, [binary,{packet, 0}]),
        timer:sleep(500),
        ok = gen_tcp:send(Socket,<<17:?UINT,0:?UINT,0:?UINT,0:?UINT>>), % HVER
        timer:send_interval(10*1000, {timer}),
        loop(Socket,<<>>).

loop(Socket,Buffer) ->
        receive
                {timer} -> ok = gen_tcp:send(Socket,<<26:?UINT,0:?UINT,0:?UINT,0:?UINT>>),
                        ?MODULE:loop(Socket,Buffer);
                {tcp,_,<<Command:?UINT,P1:?UINT,P2:?UINT,P3:?UINT>>} ->
                        io:format("~p got msg: ~p, ~p, ~p, ~p~n",[?MODULE,Command,P1,P2,P3]),
                        ?MODULE:loop(Socket,Buffer);
                Any -> io:format("~p got unknown msg: ~p~n",[?MODULE, Any]),
                        ?MODULE:loop(Socket,Buffer)
 %               after 60*1000 -> exit(timeout) % Uncomment if you want to restart this module if silent for too long
        end.
