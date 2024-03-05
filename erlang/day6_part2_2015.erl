-module(task).
-export([call/0]).

call() ->
    {ok, Input} = file:read_file("input.txt"),
    Lines = binary:split(Input, <<"\n">>, [global, trim]),
    Lights = process_lines(Lines, array:new(1000000, {default, 0})),
    Answer = array:foldl(fun(_, V, Acc) -> V + Acc end, 0, Lights),
    io:format("~p~n", [Answer]).

process_lines([], Lights) -> Lights;
process_lines([Line | Rest], Lights) ->
    [Cmd, X1, Y1, X2, Y2] = parse_line(Line),
    UpdatedLights = apply_command(Cmd, X1, Y1, X2, Y2, Lights),
    process_lines(Rest, UpdatedLights).

parse_line(Line) ->
    case binary:split(Line, <<" ">>, [global, trim]) of
        [<<"turn">>, <<"on">>, Start, <<"through">>, End] ->
            [X1, Y1] = parse_coords(Start),
            [X2, Y2] = parse_coords(End),
            [on, X1, Y1, X2, Y2];
        [<<"turn">>, <<"off">>, Start, <<"through">>, End] ->
            [X1, Y1] = parse_coords(Start),
            [X2, Y2] = parse_coords(End),
            [off, X1, Y1, X2, Y2];
        [<<"toggle">>, Start, <<"through">>, End] ->
            [X1, Y1] = parse_coords(Start),
            [X2, Y2] = parse_coords(End),
            [toggle, X1, Y1, X2, Y2]
    end.

parse_coords(Coords) ->
    [X, Y] = binary:split(Coords, <<",">>),
    [binary_to_integer(X), binary_to_integer(Y)].

apply_command(on, X1, Y1, X2, Y2, Lights) ->
    update_lights(X1, Y1, X2, Y2, Lights, fun(V) -> V + 1 end);
apply_command(off, X1, Y1, X2, Y2, Lights) ->
    update_lights(X1, Y1, X2, Y2, Lights, fun(V) -> max(V - 1, 0) end);
apply_command(toggle, X1, Y1, X2, Y2, Lights) ->
    update_lights(X1, Y1, X2, Y2, Lights, fun(V) -> V + 2 end).

update_lights(X1, Y1, X2, Y2, Lights, UpdateFun) ->
    lists:foldl(fun(Y, AccLights) ->
                    lists:foldl(fun(X, Acc) ->
                                    Index = Y * 1000 + X,
                                    array:set(Index, UpdateFun(array:get(Index, Acc)), Acc)
                                end, AccLights, lists:seq(X1, X2))
                end, Lights, lists:seq(Y1, Y2)).
