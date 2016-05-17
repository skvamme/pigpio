-module(pigpio).
-author(skvamme).
-export([start/0,init/0,loop/1]).
-define(PIGPIO_IP,"192.168.0.20").
-define(PIGPIO_PORT,8888).
-define(UINT,32/little).

start() -> Pid = spawn_link(?MODULE,init,[]), {ok,Pid}.

init() ->
        io:format("New process: ~p~n", [?MODULE]),
        inets:start(),
        {ok,Socket} = gen_tcp:connect(?PIGPIO_IP, ?PIGPIO_PORT, [binary,{packet, 0}]),
        ok = gen_tcp:send(Socket,c(setmode,25,0)),
        ok = gen_tcp:send(Socket,c(getmode,25)),
        callback:make(self(),33554432), % Start a callback to monitor pin 25 on a dedicated socket
        %timer:send_interval(10*1000, {timer}),
        loop(Socket).

loop(Socket) ->
        receive
                % {timer} -> ok = gen_tcp:send(Socket,c(br1)),
                %         ?MODULE:loop(Socket);
                {tcp,Socket,Data} ->
                        parse(Socket,Data),
                        ?MODULE:loop(Socket);
                {kwh,Sec} -> Kw = Sec / 1000000 * 1.6,
                        io:format("kW: ~p~n",[Kw]),
                        ?MODULE:loop(Socket);
                Any -> io:format("~p got unknown msg: ~p~n",[?MODULE, Any]),
                        ?MODULE:loop(Socket)
        end.

%******************************************************************************
% pigpio response
%******************************************************************************
parse(_Socket,<<>>) -> ok;
parse(Socket,<<0:?UINT,P1:?UINT,P2:?UINT,0:?UINT,Rest/binary>>) ->
         io:format("setmode pin ~p mode ~p~n",[P1,P2]),
         parse(Socket,Rest);
parse(Socket,<<1:?UINT,P1:?UINT,_P2:?UINT,P3:?UINT,Rest/binary>>) ->
         io:format("getmode pin ~p mode ~p~n",[P1,P3]),
         parse(Socket,Rest);
parse(Socket,<<2:?UINT,P1:?UINT,P2:?UINT,0:?UINT,Rest/binary>>) ->
         io:format("setpullupdown pin ~p pud ~p~n",[P1,P2]),
         parse(Socket,Rest);
parse(Socket,<<3:?UINT,_P1:?UINT,_P2:?UINT,P3:?UINT,Rest/binary>>) ->
         io:format("read ~p~n",[P3]),
         parse(Socket,Rest);
parse(Socket,<<4:?UINT,P1:?UINT,P2:?UINT,0:?UINT,Rest/binary>>) ->
         io:format("write pin ~p level ~p~n",[P1,P2]),
         parse(Socket,Rest);
parse(Socket,<<10:?UINT,_P1:?UINT,_P2:?UINT,P3:?UINT,Rest/binary>>) ->
         io:format("readbits 0-31 ~.2B~n",[P3]),
         parse(Socket,Rest);
parse(Socket,<<17:?UINT,_P1:?UINT,_P2:?UINT,P3:?UINT,Rest/binary>>) ->
         io:format("hver ~p~n",[P3]),
         parse(Socket,Rest);
parse(_Socket,Response) -> io:format("Error: ~w~n",[Response]).

%******************************************************************************
% pigpio commands
%******************************************************************************
%c(hver) -> <<17:?UINT,0:?UINT,0:?UINT,0:?UINT>>;
%c(br1) -> <<10:?UINT,0:?UINT,0:?UINT,0:?UINT>>;
c(read,Gpio) -> <<3:?UINT,Gpio:?UINT,0:?UINT,0:?UINT>>;
c(getmode,Gpio) -> <<1:?UINT,Gpio:?UINT,0:?UINT,0:?UINT>>.
c(setmode,Gpio,Mode) -> <<0:?UINT,Gpio:?UINT,Mode:?UINT,0:?UINT>>; % Input = 0, Output = 1
c(write,Gpio,Level) -> <<4:?UINT,Gpio:?UINT,Level:?UINT,0:?UINT>>;
c(setpullupdown,Gpio,Pud) -> <<2:?UINT,Gpio:?UINT,Pud:?UINT,0:?UINT>>. % Off = 0, Down = 1, Up = 2




