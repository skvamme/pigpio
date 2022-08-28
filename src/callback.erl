-module(callback).
-author(skvamme).
%-export([make/2,init/2,loop/4]).
-compile(export_all).
-define(PIGPIO_IP,"127.0.0.1").
-define(PIGPIO_PORT,8888).

make(Parent,Bits) -> 
	    spawn(?MODULE,init,[Parent,Bits]).

init(Parent,Bits) ->
        io:format("New process: ~p~n", [?MODULE]),
        {ok,Socket} = gen_tcp:connect(?PIGPIO_IP, ?PIGPIO_PORT, [binary,{packet, 0}]),
        ok = gen_tcp:send(Socket,<<99:32/little,0:32/little,0:32/little,0:32/little>>), % notifyopenib
		ok = gen_tcp:send(Socket,<<19:32/little,0:32/little,Bits:32/little,0:32/little>>), % notifybegin
        loop(Parent,Socket,0,infinity).

loop(Parent,Socket,Tick,Timeout) ->
        receive
            {tcp,Socket,<<_Seq:16/little,_Flags:16/little,Tick1:32/little,_Level:32/little,_Rest/binary>>} when Timeout == infinity ->
            	%io:format("callback ~w, ~w, ~w, ~.2B~n",[Seq,Flags,Tick1,Level]),
            	Parent ! {kwh,(Tick1 - Tick)},
                ?MODULE:loop(Parent,Socket,Tick1,50);
            _Any -> %io:format("~p got unknown msg: ~p~n",[?MODULE, Any]),
                ?MODULE:loop(Parent,Socket,Tick,Timeout)
        after Timeout ->
        	?MODULE:loop(Parent,Socket,Tick,infinity)
        end.
