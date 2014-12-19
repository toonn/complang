%
%   Toon Nolten
%
-module(tile).

-export([tilemain/2]).

%tilemain( Id ) ->
%	tilemain(Id, 0).

tilemain( Id, Value ) ->
    process_flag(trap_exit, true),
    glob:registerName(glob:regformat(Id), self()),
    tilelife(Id, Value, false).

opposite_dir(up) -> dn;
opposite_dir(dn) -> up;
opposite_dir(lx) -> rx;
opposite_dir(rx) -> lx.

dirdelta(up) -> -4;
dirdelta(dn) ->  4;
dirdelta(lx) -> -1;
dirdelta(rx) ->  1.

inbound(_, PossId, up) ->
    PossId > 0;
inbound(_, PossId, dn) ->
    PossId < 17;
inbound(Id, PossId, Dir) ->
    LRextreme = ((Id - 1) div 4) * 4,
    case Dir of
        lx -> PossId > 1 + LRextreme - 1;
        rx -> PossId < 4 + LRextreme + 1
    end.

nextinline(Id, Direction) ->
    Next = Id + dirdelta(opposite_dir(Direction)),
    Inbound = inbound(Id, Next, opposite_dir(Direction)),
    if
        Inbound ->
            Next;
        true ->
            noid
    end.

dirlocations(Id, Direction) ->
    Next = Id + dirdelta(Direction),
    Inbound = inbound(Id, Next, Direction),
    if
        Inbound ->
            [Id | dirlocations(Next, Direction)];
        true ->
            [Id]
    end.

furthest(_, Furthest, []) ->
    Furthest;
furthest(Value, Furthest, [H | T]) -> 
    Hpid = whereis(glob:regformat(H)),
    link(Hpid),
    Hpid ! {yourValue, self()},
    receive
        {tilevalue, _, HVal, false} ->
            if
                0 == HVal ->
                    clean_unlink(Hpid),
                    furthest(Value, {H, 0}, T);
                Value == HVal ->
                    clean_unlink(Hpid),
                    {H, HVal};
                true ->
                    clean_unlink(Hpid),
                    Furthest
            end;
        {tilevalue, _, _, true} ->
            clean_unlink(Hpid),
            Furthest;
        {'EXIT', Hpid, _} ->
            clean_unlink(Hpid),
            furthest(Value, Furthest, [H | T])
    end.
furthest(Value, [H | Dirlocations]) ->
    furthest(Value, {H, Value}, Dirlocations).


clean_unlink(Pid) ->
    unlink(Pid),
    receive {'EXIT', Pid, _} -> true after 0 -> true end.
    

move_dir(Id, CurrentValue, Direction) ->
    case furthest(CurrentValue, dirlocations(Id, Direction)) of
        {Id, _} -> 
            NValue = CurrentValue;
        {DestId, 0} ->
            manager ! {newvalue, DestId, CurrentValue},
            certainly(glob:regformat(DestId), {setvalue, CurrentValue, false}),
            NValue = 0;
        {DestId, CurrentValue} ->
            manager ! {newvalue, DestId, 2*CurrentValue},
            certainly(glob:regformat(DestId), {setvalue, 2*CurrentValue, true}),
            NValue = 0
    end,
    case nextinline(Id, Direction) of
        noid -> ok;
        Next -> certainly(glob:regformat(Next), Direction)
    end,
    NValue.

certainly(Id, Message) ->
    try
        Id ! Message
    catch
        _:_ ->
            certainly(Id, Message)
    end.

tilelife(Id, CurrentValue, Merged)->
	receive
		die ->
			debug:debug("I, ~p, die.~n",[Id]),
			exit(killed);
		up ->
            NValue = move_dir(Id, CurrentValue, up),
            manager ! {moveFinish, Id},
            tilelife(Id, NValue, false);
		dn ->
            NValue = move_dir(Id, CurrentValue, dn),
            manager ! {moveFinish, Id},
            tilelife(Id, NValue, false);
		lx ->
            NValue = move_dir(Id, CurrentValue, lx),
            manager ! {moveFinish, Id},
            tilelife(Id, NValue, false);
		rx ->
            NValue = move_dir(Id, CurrentValue, rx),
            manager ! {moveFinish, Id},
            tilelife(Id, NValue, false);
		{yourValue, Repl} ->
			certainly(Repl, {tilevalue, Id, CurrentValue, Merged}),
            tilelife(Id, CurrentValue, Merged);
		{setvalue, Future, NewMerged} ->
			tilelife(Id, Future, NewMerged)
	end.
