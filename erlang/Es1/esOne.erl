-module(esOne).

-export([start/1, func/1]).

start(Par) ->
	io:format("Client: I am ~p, i was spawned by the server: ~p \n",[self(),Par]),

	%fill this part!

	ok.



func(Parent)->
	io:format("Child: I am ~p, i was spawned from ~p \n",[self(),Parent]).