-module(dummy).

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
    dummy ! {register, self(), killer},
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
            srv:logSer("[~p] I attack with power ~p.\n", [killer, Atk], killer),
            dummy ! {atk, self(), killer, Atk},
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
            dummy ! {killed, self(), killer},
			ok					%quit
	end.
