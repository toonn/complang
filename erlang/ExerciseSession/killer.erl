-module(killer).

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

test()->
    register(killer, self()),
    intermittent ! {register, self(), killer},
    receive
        {vals, Atk, Def} ->
            spawn(srv, striker, [killer]),
            loop(Atk, Def, 20)
    end.

%%
%% Local Functions
%%

loop(Atk, Def, Hp)->
	receive
        {comm, What, Arg} ->
            srv:logSer(What, Arg, killer);
        strike ->
            try
            srv:logSer("[~p] I attack with power ~p.\n", [killer, Atk], killer),
            intermittent ! {atk, self(), killer, Atk} of
                _ -> ok
            catch
                error:Throw -> 
                    srv:logSer("[~p] My attack was unsuccesful.\n", [killer],
                               killer),
                    {throw, caught, Throw}
            end,
            loop(Atk, Def, Hp);
        {atk, _, _, Power} ->
            RHp = Hp - Power + Def,
            srv:logSer("[~p] I was hit with power ~p, I have ~p hp left.\n", [killer, Power, RHp], killer),
            if
                RHp =< 0 -> killer ! die, loop(Atk, Def, RHp);
                true -> loop(Atk, Def, RHp)
            end;
		die ->
			srv:logSer("[~p] Alas, i die.\n", [killer], killer),
            try intermittent ! {killed, self(), killer} of
                _ -> ok
            catch
                error:Throw -> {throw, caught, Throw}
            end,
			ok					%quit
	end.
