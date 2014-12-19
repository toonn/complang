-module(manager).

-export([manage/0]).

spawn_tiles(TileIds, TileValues) ->
    lists:map(fun(Id) ->
                spawn_link(tile, tilemain, [Id, element(Id, TileValues)])
              end, TileIds).

await_finish([]) -> ok;
await_finish(Await) ->
    receive
        {moveFinish, Id} ->
            await_finish(lists:delete(Id, Await))
    end.
await_finish() ->
    await_finish(lists:seq(1,16)).

restore_tiles(TileTuple) ->
    Registered = registered(),
    spawn_tiles(lists:filter(fun(El) ->
                                not lists:member(glob:regformat(El),
                                Registered)
                             end,
                lists:seq(1,16)), TileTuple).

safemsg(Id, Message, TileTuple) ->
    try
        glob:regformat(Id) ! Message
    catch
        _:_ ->
            restore_tiles(TileTuple),
            safemsg(Id, Message, TileTuple)
    end.

manage() ->
    process_flag(trap_exit, true),
    InitialTileValues = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    spawn_tiles(lists:seq(1,16), InitialTileValues),
	manageloop(InitialTileValues, true, true).

% when receiving the message $senddata, spaw a collector and a broadcaster for the collection of the data
%  from the tiles. Then, once the $Data is collected, inform the lifeguard and the gui
manageloop(TileTuple, Changed, CollectAble) ->
	receive
        {'EXIT', _, killed} ->
            NCollectAble = CollectAble,
            NChanged = Changed,
            NTileTuple = TileTuple,
            restore_tiles(TileTuple);
        {newvalue, Id, Value} ->
            NCollectAble = CollectAble,
            NChanged = true,
            NTileTuple = setelement(Id, TileTuple, Value);
		up ->
            NCollectAble = false,
            NChanged = Changed,
            NTileTuple = TileTuple,
			Tmp = [1,2,3,4],
			lists:map(fun(X) -> safemsg(X, up, NTileTuple) end, Tmp);
		dn ->
            NCollectAble = false,
            NChanged = Changed,
            NTileTuple = TileTuple,
			Tmp = [13,14,15,16],
			lists:map(fun(X) -> safemsg(X, dn, NTileTuple) end, Tmp);
		lx ->
            NCollectAble = false,
            NChanged = Changed,
            NTileTuple = TileTuple,
			Tmp = [1,5,9,13],
			lists:map(fun(X) -> safemsg(X, lx, NTileTuple) end, Tmp);
		rx ->
            NCollectAble = false,
            NChanged = Changed,
            NTileTuple = TileTuple,
			Tmp = [4,8,12,16],
			lists:map(fun(X) -> safemsg(X, rx, NTileTuple) end, Tmp);
		sendData ->
            NChanged = Changed,
            NTileTuple = TileTuple,
			Basetuple = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
			%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
			% this is the instruction mentioned in the text %
			%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
            if
                not CollectAble -> await_finish();
                true -> ok
            end,
            NCollectAble = true,
			%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
			% this is the instruction mentioned in the text %
			%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
			PidCollector = spawn( fun() -> collect( 0, Basetuple ) end),
			register( collector, PidCollector ),
			spawn( fun() ->
                   broadcaster( 16, {yourValue, collector},NTileTuple ) end);
		{collectedData, TupleData} ->
            NCollectAble = CollectAble,
            % This if makes sure the rules follow those in the actual game:
            % if nothing moves, no new random tiles are added.
            if
                Changed ->
                    NChanged = false,
                    ListData = randomiseatile(TupleData, TupleData),
                    NTileTuple = list_to_tuple(ListData);
                true ->
                    NChanged = Changed,
                    NTileTuple = TileTuple,
                    ListData = tuple_to_list(TileTuple)
            end,
			gui ! {values, ListData}
	end,
	manageloop(NTileTuple, NChanged, NCollectAble).

% takes a tuple of data in input and returns it in a list format
% with an element that was 0 now randomized to 2 or 4
randomiseatile( Tuple, TileTuple )->
	{A1,A2,A3} = now(),
    random:seed(A1, A2, A3),
	case glob:zeroesintuple(Tuple) of
		0 ->
			Tu = Tuple;
		_ ->
			C1 = getCand(0, Tuple),
			V1 = 2*random:uniform(2),
			debug:debug("MANAGER: radomised in ~p.~n",[C1]),
			safemsg(C1, {setvalue, V1, false}, TileTuple),
			Tu = erlang:setelement(C1,Tuple,V1)
	end,
	erlang:tuple_to_list(Tu).

% returns a number from 1 to 16 different from $Oth
	% such that its value in $T is 0, i.e. $return can be initialised at random
getCand( Oth , T)->
	C = random:uniform(16),
	case C of
		Oth -> getCand(Oth, T);
		_ ->
			case erlang:element(C, T) of
				0 -> C;
				_ -> getCand(Oth, T)
			end
	end.

% collects 16 numbes in $T, then returns the related tuple
%	$T is a tuple of length 16
collect( N , T) ->
	case N of
		16 -> 
			manager ! {collectedData, T};
		Num ->
			receive
				{tilevalue, Id, Value, _} ->
					collect( Num+1, erlang:setelement(Id, T, Value))
			end
	end.

% Sends message $Mess to all tiles
broadcaster( 0, _, _ )->
	ok;
broadcaster( N, Mess, TileTuple ) when N < 17 -> 
	try safemsg(N, Mess, TileTuple) of
		_ -> 
			%debug:debug("broadcasting to ~p.~n",[N]),
			broadcaster(N-1, Mess, TileTuple)
	catch
		_:_ -> 
            broadcaster(N, Mess, TileTuple)
			%debug:debug("BROADCASTER: cannot commmunicate to ~p. Error ~p.~n",[N,F])
	end.
