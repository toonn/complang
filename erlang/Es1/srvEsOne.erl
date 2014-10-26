%% author: squera
-module(srvEsOne).

%%
%% export functions
%%

-export([start/0]).

%%function definition

start()->
	io:format("Server: Starting at pid: ~p \n",[self()]),
	case lists:member(serverEsOne, registered()) of
		true ->
			unregister(serverEsOne);							%if the token  is present, remove it
		false ->
			ok
	end,
	register(serverEsOne,self()),
	Pid = spawn(esOne, start,[self()]),
	loop(false, false,Pid).

%
loop(Prec, Nrec,Pd)->
	case Prec of
		true ->
			case Nrec of
				true -> 	
					io:format("Server: I reply to ~p \n",[Pd]),
					Pd ! {reply, self()},
					io:format("Server: I quit \n",[]),
					ok;
				false ->
					receiveLoop(Prec,Nrec,Pd)
			end;
		false ->
			receiveLoop(Prec,Nrec,Pd)
	end.

receiveLoop(Prec,Nrec,Pid) ->
	receive
		{onPid, Pid}->
			io:format("Server: I received a message to my pid from ~p \n",[Pid]),
			loop(true, Nrec,Pid);
		{onName,Pid}->
			io:format("Server: I received a message to name from ~p \n",[Pid]),
			loop(Prec,true,Pid)
	after
		5000->
			io:format("Server: I received no messages, i quit\n",[]),
			ok
	end.

