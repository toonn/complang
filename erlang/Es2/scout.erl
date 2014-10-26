%% Author: squera
%% Created: Aug 10, 2011
%% Description: TODO: Add description to scout
-module(scout).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([scout/2,inform/1]).
%%
%% API Functions
%



scout(Name,Par)->
	P=spawn(scout,inform,[self()]),
	logSco("I ~p start.~n", [Name],Name),
	startScout(Name,Par,[],[],P).


startScout(Name,Parent,Knowledge,Dead,P)->			
	receive	
		die->
			logSco("I die.~n~n", [],Name),
			P!die,
			ok;
		look->
			AllReg= lists:filter(fun(X) -> filtro(X)== true end, registered()),						% all the registered that start with erLab_
			AllNoDead = remAll(Dead,AllReg),												% remove all dead elements			
			AllNoPar = lists:delete(Parent, AllNoDead),										% remove the master
			AllNoDup=remDupList(AllNoPar),													% eliminate internal duplicates
			OnlyNew = newEntries(AllNoDup,Knowledge),										% detect new entries
			tell(Parent,Name,OnlyNew),
			New= lists:append(Knowledge, OnlyNew),
			logSco("New list: ~p, is old list:~p plus the new one:~p.~n \t Dead procs: ~p.~n", [New,Knowledge,OnlyNew,Dead],Name),
			startScout(Name,Parent,New,Dead,P);
		{remove, ToRem}->
			NewDead=Dead++[ToRem],														%add the name to the dead people
			NewKno=lists:delete(ToRem, Knowledge),
			logSco("Proc ~p died, dead procs were ~p.~n", [ToRem,Dead],Name),
			startScout(Name,Parent,NewKno,NewDead,P)
	end.


inform(P)->					% tells when to look
	receive
		die->
			ok
	after
			200->			%  after 200 msec, tell P to look
			try P ! look
			catch
				_:_->
					ok
			end,
			inform(P)
	end.




%%
%% Local Functions
%%
			
remDups(El,List)->								%removes all occurrences of El from List
	case List of
		[H|T]->
			case El==H of
				true->
					remDups(El,T);
				false->
					[H]++remDups(El,T)
			end;
		[]->
			[]
	end.

remDupList(List)->								%removes all duplicates from a list
	case List of
		[H|T]->
			[H]++remDupList(remDups(H,T));
		[]->
			[]
	end.

remAll(Who,From)->						%removes elements of Who from From
	case Who of
		[H|T]->
			L=lists:delete(H, From),
			remAll(T,L);
		[]->
			From
	end.


filtro(El)->
	case re:run(erlang:atom_to_list(El), "(erLab_).*",[]) of	%convert the atom into string and see if it 
		{match, _} ->											%starts with erLab_
			true;
		nomatch ->
			false
	end.
		
newEntries(L1,L2)->
	case L1 of
		[H|T]->									%recursively look into L1
			case lists:member(H, L2) of			% if the head is in L2
				true ->
					newEntries(T,L2);
				false ->
					[H]++newEntries(T,L2)		%name to be added if new
			end;
		[]->									%return empty if L1 is empty
			[]
	end.

tell(Dest,Who,What)->						%recursively scan the list of names to send
	case What of
		[H|T]->
			Dest ! {addEnemy, Who, H},		%attacker will receive the names on token addEnemy
			tell(Dest,Who,T);
		[]->
			ok
	end.



logSco(What,Arg,Name)->
	srv:logSer(What, Arg, Name).

