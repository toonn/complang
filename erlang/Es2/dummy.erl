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
	io:format("This is how you print with arguments ~p ~p.\n",[self(),9]),
	loop(1).

%%
%% Local Functions
%%

loop(State)->
	receive
		dummy->					% tail recursive call
			io:format("This is loop number ~p.\n",[State]),
			loop(State+1);
		die->
			io:format("Alas, i die."),
			ok					%quit
	end.
