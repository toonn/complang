%% Author: squera
%% Created: Aug 26, 2011
%% Description: TODO: Add description to srv
-module(srv).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([startD/0,startM/0,listenD/4,listenS/5,alternator/2,striker/1,striker/2,startO/0,startK/0,start/1,listenO/3,waiter/1,listenI/2,startI/0,logSer/3]).

%%
%% API Functions
%%


%various starters for different servers

startD()->
	start(dummy).
startO()->
	start(officialServer).
startK()->
	start(knightServer).
startI()->
	start(intermittent).
startM()->
	start(maelstromServer).

%general function for starting a server

start(Tok)->
	case lists:member(Tok, registered()) of
		true ->
			unregister(Tok);							%if the token  is present, remove it
		false ->
			ok
	end,
	case Tok of
		dummy->
			Pid = spawn(srv,listenD,[0,0,0,Tok]);
		officialServer->
			Pid = spawn(srv,listenO,[Tok,[],0]),
			Wt = spawn(srv,waiter,[Tok]),						% waiter that tells me when to die
			Pid ! {reg, Wt};
		maelstromServer->
			Pid = spawn(srv,listenO,[Tok,[],0]),
			Wt = spawn(srv,waiter,[Tok]),						% waiter that tells me when to die
			Pid ! {reg, Wt};
		knightServer->
			Pid = spawn(srv,listenS,[0,[],0,0,Tok]),
			spawn(srv,striker,[Tok]);
		intermittent->
			Pid = spawn(srv,listenI,[Tok,0])
	end,
	logSer("[Srv]\t[~p]: \t I, ~p, start.~n",[now(),Tok],Tok),
	register(Tok,Pid).										%register the token .


%-----------------------------------------------------%
%-----------------------------------------------------%
%--------------   dummy server  ----------------------%
%-----------------------------------------------------%
%-----------------------------------------------------%

listenD(N,D,A,Tok)->
	receive
		die->
			logSer("[Srv]\t I die.~n",[],Tok),
			ok;
		{register, Pid, Name}->
			logSer("[Srv]\tI register ~p with name ~p at time ~p.~n",[Pid,Name,now()],Tok),
			random:seed(now()),							%reseed the random generator
			Att=5+random:uniform(5),
			Def=random:uniform(5),
			logSer("[Srv]\t I send ~p and ~p to ~p at time ~p.~n",[Att,Def,Pid,now()],Tok),
			case lists:member(Name, registered()) of							%check if they registered the proc
				true->
					Name ! {vals, Att, Def};									% communicate the values to the process. 
				false->
					Pid ! {comm, "You are not registered, i cannot find ~p in the list.~n",[Name]}
			end,
			spawn(srv,striker,[Tok]),										%process that helps me killin'
			listenD(Name,Def,Att,Tok);
		{atk, _, Name, Val}->
			logSer("[Srv]\t I was attacked by ~p with ~p at ~p. ~n",[Name,Val,now()],Tok),
			case (A==Val) of											% check that they do not cheat
				true-> 
					ok;
				false ->
					Name ! {comm, "You are cheating,you should have ~p power instead of ~p!~n",[A,Val]}
			end,
			listenD(N,D,A,Tok);
		strike ->														% when i receive this, it's time to strike!
			logSer("[Srv]\t I attack ~p with ~p damage.~n",[N,D+10],Tok),
			N ! {atk, self(), Tok, D+10},
			listenD(N,D,A,Tok);
		{killed, Pid, Name}->											%ok, i killed that process
			logSer("[Srv]\t I killed ~p with name ~p.~n",[Pid,Name],Tok),
			self() ! die,
		listenD(N,D,A,Tok)
	end.


striker(Tok,Time)->						%process that helps me. it sends me a message every Time msec telling me to attack
	receive
		die -> ok
	after
		Time->
			try Tok ! strike
			catch
				_:_->
					self() ! die
			end,
			striker(Tok)
	end.

striker(Tok)->
	striker(Tok,200).


wait(T)->
	receive
		after
			T->
			ok
	end.

%-----------------------------------------------------%
%-----------------------------------------------------%
%-------   intermittent server  ----------------------%
%-----------------------------------------------------%
%-----------------------------------------------------%


alternator(P,Tot)->
	wait(300),
	P!wake,
	case Tot>10000 of
		true-> P! die;
		false-> wait(500),
				P! sleep,
				alternator(P,Tot+800)
	end.



listenI(Name,C)->
	receive
		{register, Pid, NN}->
			logSer("[Srv]\tI register ~p with name ~p at time ~p.~n",[Pid,NN,now()],Name),
			random:seed(now()),							%reseed the random generator
			Att=5+random:uniform(5),
			Def=random:uniform(5),
			logSer("[Srv]\t I send ~p and ~p to ~p at time ~p.~n",[Att,Def,Pid,now()],Name),
			case lists:member(NN, registered()) of							%check if they registered the proc
				true->
					NN ! {vals, Att, Def};									% communicate the values to the process. 
				false->
					Pid ! {comm, "You are not registered, i cannot find ~p in the list.~n",[NN]}
			end,
			unregister(Name),
			PPP=self(),
			spawn(srv,alternator,[PPP,0]),
			listenI(Name,C);
		sleep->
			unregister(Name),
			logSer("[Srv]\tAt ~p i unregister.~n",[now()],Name),
			listenI(Name,C);
		wake->
			register(Name,self()),
			logSer("[Srv]\tAt ~p i register.~n",[now()],Name),
			listenI(Name,C);
		{atk, P,_,_}->
			Count=C+1,
			logSer("[Srv]\t\t At ~p i was hit!.~n",[now()],Name),
			case Count==4 of
				true->
					P! {atk, self(), Name, 4000},
					self() ! die;
				false->
					ok
			end,
			listenI(Name,Count);
		die->
			unregister(Name),
			logSer("[Srv]\tAt ~p i am told to die.~n~n",[now()],Name),
			ok
	end.


%-----------------------------------------------------%
%-----------------------------------------------------%
%--------------  knight server  ----------------------%
%-----------------------------------------------------%
%-----------------------------------------------------%


listenS(N,SS,D,A,Tok)->
	receive
		die->
			logSer("[Srv]\tI die.",[],Tok);
		{register, Pid, Name}->
			logSer("[Srv]\tI register ~p with name ~p at time ~p.~n",[Pid,Name,now()],Tok),
			random:seed(now()),							%reseed the random generator
			Att=5+random:uniform(5),
			Def=random:uniform(5),
			logSer("[Srv]\t I send ~p and ~p to ~p at time ~p.~n",[Att,Def,Pid,now()],Tok),
			case lists:member(Name, registered()) of							%check if they registered the proc
				true->
					case N==0 of
						true->
							Name ! {vals, Att, Def};									% communicate the values to the process.
						false->
							ok
					end;
				false->
					Pid ! {comm, "You are not registered, i cannot find ~p in the list.~n",[Name]}
			end,										%process that helps me killin'
			listenS(Name,SS,Def,Att,Tok);
		{registerKnight, PidKn, NameKn,Master}->
			logSer("[Srv]\tI register a knight ~p with name ~p, whose master is ~p at time ~p.~n",[PidKn,NameKn,Master,now()],Tok),
			random:seed(now()),							%reseed the random generator
			Att=5+random:uniform(5),
			Def=random:uniform(5),
			logSer("[Srv]\t I send ~p and ~p to ~p at time ~p.~n",[Att,Def,PidKn,now()],Tok),
			case lists:member(NameKn, registered()) of							%check if they registered the proc
				true->
					S=SS++[NameKn],
					NameKn ! {vals, Att, Def};									% communicate the values to the process. 
				false->
					S=SS,
					PidKn ! {comm, "You are not registered, i cannot find ~p in the list.~n",[NameKn]}
			end,
			listenS(N,S,Def,Att,Tok);
		{atk, _, Name, Val}->
			logSer("[Srv]\t \t I was attacked by ~p with ~p at ~p. ~n",[Name,Val,now()],Tok),
			listenS(N,SS,D,A,Tok);
		strike ->														% when i receive this, it's time to strike!
			attackAll(N,SS,Tok),
			listenS(N,SS,D,A,Tok);
		{killed, Pid, Name}->											%ok, i killed that process
			logSer("[Srv]\t I killed ~p with name ~p.~n",[Pid,Name],Tok),
			S=lists:delete(Name, SS),
			case Name==N of
				false->
					ok;
				true->
					self() ! die
			end,
			listenS(N,S,D,A,Tok)
	end.
						
%function for the squire server to attack all registered procs

attackAll(P,S,Tok)->
	R=10+random:uniform(10),
	case S of
		[H|T]->
			try H ! {atk, self(), Tok, R}
			catch
				error:_->
					ok
			end,
			logSer("[Srv]\t I attack ~p with ~p strength.~n",[H,R],Tok),
			attackAll(P,T,Tok);
		[]->
			case P==0 of
				true->
					ok;
				false->
					try P ! {atk, self(), Tok, R}
					catch
						error:_->
							self() ! {killed, P, P}
					end,
					logSer("[Srv]\t I attack ~p with ~p strength.~n",[P,R],Tok)
			end
	end.


%-----------------------------------------------------%
%-----------------------------------------------------%
%--------   official/maelstrom server  ---------------%
%-----------------------------------------------------%
%-----------------------------------------------------%

listenO(Tok,Procs,Wt)->
	case Wt==0 of 
		false ->
			Wt2=Wt,
			Wt ! renew;
		true->
			Wt2= spawn(srv,waiter,[Tok])
	end,
	receive
		{reg, W}->
			listenO(Tok,Procs,W);
		{register, Pid, Name}->
			logSer("[Srv]\t [~p]:~n\t\t I register ~p with name ~p.~n",[now(),Pid,Name],Tok),
			random:seed(now()),							%reseed the random generator
			Att=5+random:uniform(5),
			Def=random:uniform(5),
			logSer("[Srv]\t [~p]:~n\t\t I send ~p and ~p to ~p.~n",[now(),Att,Def,Pid],Tok),
			case lists:member(Name, registered()) of							%check if they registered the proc
				true->
					%PP=Procs++[{Name,Pid,Att,Def}],
					PP=Procs++[Name],
					Name ! {vals, Att, Def};									% communicate the values to the process. 
				false->
					PP=Procs,
					logSer("[Srv]\t [WARNING]:~n\t\t Process ~p is not registered with name ~p. ~n",[Pid,Name],Tok)
			end,
			logSer("[Srv]\t [~p]:~n\t\t I actually have ~p players.~n",[now(),PP],Tok),
			listenO(Tok,PP,Wt2);
		{atkLog, Pid, Name, TargetName, WeaponPower}->
			logSer("[Srv]\t [~p]:~n\t\t Process ~p with name ~p attacked ~p with ~p.~n",[now(),Pid,Name,TargetName,WeaponPower],Tok),
			listenO(Tok,Procs,Wt2);
		{killLog, Pid, Name}->										
			logSer("[Srv]\t [~p]:~n\t\t Process ~p with name ~p was killed.~n",[now(),Pid,Name],Tok),
			P=lists:delete(Name, Procs),
			tellAll(P,Name,Tok),
			listenO(Tok,P,Wt2);
		die->
			killAll(Procs),				%kill all fighters
			logSer("[Srv]\t[~p]: \t No one is here, i die.~n~n",[now()],Tok);
		{registerSquire, PidSquire, NameSquire,Master}->
			logSer("[Srv]\tI register a squire ~p with name ~p, whose master is ~p at time ~p.~n",[PidSquire,NameSquire,Master,now()],Tok),
			random:seed(now()),							%reseed the random generator
			Att=5+random:uniform(5),
			Def=random:uniform(5),
			logSer("[Srv]\t I send ~p and ~p to ~p at time ~p.~n",[Att,Def,PidSquire,now()],Tok),
			case lists:member(NameSquire, registered()) of							%check if they registered the proc
				true->
					PP=Procs++[NameSquire],
					NameSquire ! {vals, Att, Def};									% communicate the values to the process. 
				false->
					PP=Procs,
					PidSquire ! {comm, "You are not registered, i cannot find ~p in the list.~n",[NameSquire]}
			end,
			listenO(Tok,PP,Wt2);
		{dfnLog, Pid, Name, AttackerName, DefenseVal, Damage}->
			%check for no cheatings
			logSer("[Srv]\t [~p]:~n\t\t Process ~p with name ~p was attacked by ~p with ~p defense, getting ~p damage.~n",[now(),Pid,Name,AttackerName,DefenseVal,Damage],Tok),
			listenO(Tok,Procs,Wt2)
	end.

waiter(Par)->		%process that waits fo 10000 (10 secs) time and then tells the server that spawned him to die
	receive
		renew->
			waiter(Par);
		die->					% unless it is killed
			ok
	after
			10000->
			try Par ! die
			catch
				error:_-> 
					self() ! die,
					waiter(Par)
			end
	end.


killAll(P)->				% recursively scan the list and kill all processes
	case P of
		[H|T]->
			H ! die,
			killAll(T);
		[]->
			ok
	end.

tellAll(P,N,Tok)->				% recursively scan the list and inform who is dead
	case P of
		[H|T]->
			logSer("[Srv]\t [~p]:~n\t\t I tell ~p that ~p was killed.~n",[now(),H,N],Tok),
			try			H ! {killed,0, N}											% H may be killed, who knows!
			catch
				error:_->
					logSer("[Srv]\t This ~p is dead!!~n",[H],Tok)
			end,
			tellAll(T,N,Tok);
		[]->
			ok
	end.

%%
%% Local Functions
%%

logSer(What,Arg,Name)->
	Dest= string:concat("./log/", string:concat(erlang:atom_to_list(Name),"Log.txt")),
	log(Dest, What, Arg).			%make sure this exists


log(Dest,What,Arg)->
	io:format(What,Arg),								% log to a local folder
	{ok, F} = file:open(Dest, [write,append]),		
	io:format(F, What, Arg),
	file:close(F).
