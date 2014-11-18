-module(lord).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([test/0]).

%%
%% API Functions
%%

test() ->
    process_flag(trap_exit, true),
    sacrifice_knights(15, 0),
    charge().

sacrifice_knights(0, _) -> ok;
sacrifice_knights(Count, 5) ->
    receive
        {'EXIT', Id, Why} -> sacrifice_knights(Count, 4)
    end;
sacrifice_knights(Count, LiveKnights) ->
    spawn_link(knight, init, [lord, Count]),
    sacrifice_knights(Count - 1, LiveKnights + 1).

charge()->
    register(lord, self()),
    knightServer ! {register, self(), lord},
    receive
        {vals, Atk, Def} ->
            spawn(srv, striker, [lord]),
            loop(Atk, Def, 20)
    end.

%%
%% Local Functions
%%

loop(Atk, Def, Hp)->
	receive
        {comm, What, Arg} ->
            srv:logSer(What, Arg, lord);
        strike ->
            try
            srv:logSer("[~p] I attack with power ~p.\n", [lord, Atk], lord),
            knightServer ! {atk, self(), lord, Atk} of
                _ -> ok
            catch
                error:Throw -> 
                    srv:logSer("[~p] My attack was unsuccesful.\n", [lord],
                               lord),
                    {throw, caught, Throw}
            end,
            loop(Atk, Def, Hp);
        {atk, _, _, Power} ->
            RHp = Hp - Power + Def,
            srv:logSer("[~p] I was hit with power ~p, I have ~p hp left.\n",
                       [lord, Power, RHp], lord),
            if
                RHp =< 0 -> lord ! die, loop(Atk, Def, RHp);
                true -> loop(Atk, Def, RHp)
            end;
		die ->
			srv:logSer("[~p] Alas, i die.\n", [lord], lord),
            try knightServer ! {killed, self(), lord} of
                _ -> ok
            catch
                error:Throw -> {throw, caught, Throw}
            end,
			ok					%quit
	end.
