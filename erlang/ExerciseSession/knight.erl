-module(knight).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([init/2]).

%%
%% API Functions
%%

init(LordlyName, KnightlyNumber)->
    KnightlyName = list_to_atom("erlab_" ++ integer_to_list(KnightlyNumber)),
    register(KnightlyName, self()),
    knightServer ! {registerKnight, self(), KnightlyName, LordlyName},
    receive
        {vals, Atk, Def} ->
            spawn(srv, striker, [KnightlyName]),
            loop(Atk, Def, 20)
    end.

%%
%% Local Functions
%%

loop(Atk, Def, Hp)->
    {registered_name, KnightlyName} = process_info(self(), registered_name),
	receive
        {comm, What, Arg} ->
            srv:logSer(What, Arg, KnightlyName);
        strike ->
            try
            srv:logSer("[~p] I attack with power ~p.\n", [KnightlyName, Atk],
                       KnightlyName),
            knightServer ! {atk, self(), KnightlyName, Atk} of
                _ -> ok
            catch
                error:Throw -> 
                    srv:logSer("[~p] My attack was unsuccesful.\n",
                               [KnightlyName], KnightlyName),
                    {throw, caught, Throw}
            end,
            loop(Atk, Def, Hp);
        {atk, _, _, Power} ->
            RHp = Hp - Power + Def,
            srv:logSer("[~p] I was hit with power ~p, I have ~p hp left.\n",
                       [KnightlyName, Power, RHp], KnightlyName),
            if
                RHp =< 0 -> KnightlyName ! die, loop(Atk, Def, RHp);
                true -> loop(Atk, Def, RHp)
            end;
		die ->
			srv:logSer("[~p] Alas, i die.\n", [KnightlyName], KnightlyName),
            try knightServer ! {killed, self(), KnightlyName} of
                _ -> ok
            catch
                error:Throw -> {throw, caught, Throw}
            end,
			ok					%quit
	end.
