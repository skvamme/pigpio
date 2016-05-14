-module(pigpio).
-author(skvamme).
-export([start/0,init/0,loop/1]).
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
        ok = gen_tcp:send(Socket,c(hver)),
        timer:send_interval(10*1000, {timer}),
        loop(Socket).

loop(Socket) ->
        receive
                {timer} -> ok = gen_tcp:send(Socket,c(read,3)),
                        ?MODULE:loop(Socket);
                {tcp,_,<<Command:?UINT,P1:?UINT,P2:?UINT,P3:?UINT>>} ->
                        io:format("~p got msg: ~p, ~p, ~p, ~p~n",[?MODULE,Command,P1,P2,P3]),
                        ?MODULE:loop(Socket);
                Any -> io:format("~p got unknown msg: ~p~n",[?MODULE, Any]),
                        ?MODULE:loop(Socket)
 %               after 60*1000 -> exit(timeout) % Uncomment if you want to restart this module if silent for too long
        end.


%******************************************************************************
% pigpio commands
%******************************************************************************
c(hver) -> <<17:?UINT,0:?UINT,0:?UINT,0:?UINT>>.
c(read,Gpio) -> <<3:?UINT,Gpio:?UINT,0:?UINT,0:?UINT>>.
