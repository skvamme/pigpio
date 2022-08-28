-module(pigpio).
-author(skvamme).
%-export([start/0,init/0,loop/1]). 
-compile(export_all).
-define(PIGPIO_IP,"127.0.0.1").
-define(PIGPIO_PORT,8888).
-define(UINT,32/little).

start() -> Pid = spawn_link(?MODULE,init,[]), {ok,Pid}.

init() ->
        io:format("New process: ~p~n", [?MODULE]),
        inets:start(),
        {ok,Socket} = gen_tcp:connect(?PIGPIO_IP, ?PIGPIO_PORT, [binary,{packet, 0}]),
        gen_tcp:send(Socket,hver()),
        %callback:make(self(),33554432), % Start a callback to monitor pin 25 on a dedicated socket
        loop(Socket).

loop(Socket) ->
        receive
                {servo_left,Gpio} -> ok = gen_tcp:send(Socket,servo(Gpio,500)), % Caution: Try servo_center first
                         ?MODULE:loop(Socket);                        % Then walk your way down with servo_pos to find
                {servo_center,Gpio} -> ok = gen_tcp:send(Socket,servo(Gpio,1500)), % the lower limit of your servo.
                         ?MODULE:loop(Socket);
                {servo_right,Gpio} -> ok = gen_tcp:send(Socket,servo(Gpio,2500)), % Same caution here, walk your way up
                         ?MODULE:loop(Socket);                          % to find the upper limit
                {servo_pos,Gpio,Pos} -> ok = gen_tcp:send(Socket,servo(Gpio,Pos)), 
                         ?MODULE:loop(Socket);                        
                {tcp,Socket,Data} ->
                        parse(Socket,Data),
                        ?MODULE:loop(Socket);
                Any -> io:format("~p got unknown msg: ~p~n",[?MODULE, Any]),
                        ?MODULE:loop(Socket)
        end.

%***************************************************************************************
% pigpio response It's easy to add a response you are missing, read
% http://abyz.me.uk/rpi/pigpio/sif.html and http://abyz.me.uk/rpi/pigpio/pigs.html
%***************************************************************************************
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
parse(_Socket,Response) -> io:format("Unhandled response: ~w~n",[Response]).

%**********************************************************************************************
% pigpio commands. It's easy to add a command you are missing, read
% http://abyz.me.uk/rpi/pigpio/sif.html and http://abyz.me.uk/rpi/pigpio/pigs.html
%**********************************************************************************************
hver() -> <<17:?UINT,0:?UINT,0:?UINT,0:?UINT>>. % Show hardware version
read(Gpio) -> <<3:?UINT,Gpio:?UINT,0:?UINT,0:?UINT>>.  % Reads the current level of GPIO
write(Gpio,Level) -> <<4:?UINT,Gpio:?UINT,Level:?UINT,0:?UINT>>. % Sets GPIO to Level The level may be 0 or 1
setpullupdown(Gpio,Pud) -> <<2:?UINT,Gpio:?UINT,Pud:?UINT,0:?UINT>>. % Off = 0, Down = 1, Up = 2
getmode(Gpio) -> <<1:?UINT,Gpio:?UINT,0:?UINT,0:?UINT>>. % Returns the current mode of GPIO
setmode(Gpio,Mode) -> <<0:?UINT,Gpio:?UINT,Mode:?UINT,0:?UINT>>. % Input = 0, Output = 1
br1() -> <<10:?UINT,0:?UINT,0:?UINT,0:?UINT>>. % Read bank 1 GPIO
bs1(Bits) -> <<14:?UINT,Bits:?UINT>>. % Set specified GPIO in bank 1
prs(Gpio,Range) -> <<6:?UINT,Gpio:?UINT,Range:?UINT,0:?UINT>>. % sets the dutycycle Range to be used for GPIO
pwm(Gpio,Dutycycle) -> <<5:?UINT,Gpio:?UINT,Dutycycle:?UINT,0:?UINT>>. % Starts PWM on GPIO with Dutycycle
pfs(Gpio,Hz) -> <<7:?UINT,Gpio:?UINT,Hz:?UINT,0:?UINT>>. % Sets the PWM frequency Hz to be used for GPIO
servo(Gpio,Pulsewidth) -> <<8:?UINT,Gpio:?UINT,Pulsewidth:?UINT,0:?UINT>>. % Starts servo pulses of Pulsewidth microseconds on GPIO
trig(Gpio,Pulselen,Level) -> <<37:?UINT,Gpio:?UINT,Pulselen:?UINT,4:?UINT,Level:?UINT>>. % Sends a trigger pulse of Pulselen microseconds at level to GPIO
notifyopenib() -> <<99:?UINT,0:?UINT,0:?UINT,0:?UINT>>. % Requests a free notification handle. 
notifybegin(Bits) -> <<19:?UINT,0:?UINT,Bits:?UINT,0:?UINT>>. % Int where Gpio is 1, other bits 0




