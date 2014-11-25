%
%   Toon Nolten
%
-module(tile).

-export([tilemain/1]).

tilemain( Id ) ->
    glob:registerName(glob:regformat(Id), self()),
	tilemain(Id, 0).

tilemain( Id, Value ) ->
    tilelife(Id, Value, 0).

dirdelta(up) -> -4;
dirdelta(dn) ->  4;
dirdelta(lx) -> -1;
dirdelta(rx) ->  1.

inbound(_, Val, up) ->
    Val > 0;
inbound(_, Val, dn) ->
    Val < 17;
inbound(Id, Val, lx) ->
    Val > 1 + ((Id - 1) div 4) * 4 - 1;
inbound(Id, Val, rx) ->
    Val < 4 + ((Id - 1) div 4) * 4 + 1.

dirlocations(Id, Direction) ->
    Delta = dirdelta(Direction),
    Inbound = inbound(Id, Id + Delta, Direction),
    if
        Inbound ->
            [Id | dirlocations(Id + dirdelta(Direction), Direction)];
        true ->
            [Id]
    end.

furthest(Value, Furthest, [H | T]) -> 
    
furthest(_, Furthest, []) ->
    Furthest.

tilelife(Id, CurrentValue, Merged)->
	receive
		die ->
			debug:debug("I, ~p, die.~n",[Id]),
			exit(killed);
		up ->
			dirlocations(Id, up);
		dn ->
			ok;
		lx ->
			ok;
		rx ->
			ok;
		{yourValue, Repl} ->
			Repl ! {tilevalue, Id, CurrentValue, Merged},
            tilelife(Id, CurrentValue, Merged);
		{setvalue, Future, NewMerged} ->
			tilelife(Id, Future, NewMerged)
	end.
