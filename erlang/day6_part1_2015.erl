-module(task).
-export([call/0]).

call() ->
    {ok, Input} = file:read_file("input.txt"),
    Lines = binary:split(Input, <<"\n">>, [global, trim]),
    Lights = process_instructions(Lines, sets:new()),
    Answer = sets:size(Lights),
    io:format("~p~n", [Answer]).

process_instructions([], Lights) ->
    Lights;
process_instructions([Instruction | Rest], Lights) ->
    NewLights = process_instruction(Instruction, Lights),
    process_instructions(Rest, NewLights).

process_instruction(<<"turn on ", Rest/binary>>, Lights) ->
    {Start, End} = parse_coordinates(Rest),
    turn_on_lights(Start, End, Lights);
process_instruction(<<"turn off ", Rest/binary>>, Lights) ->
    {Start, End} = parse_coordinates(Rest),
    turn_off_lights(Start, End, Lights);
process_instruction(<<"toggle ", Rest/binary>>, Lights) ->
    {Start, End} = parse_coordinates(Rest),
    toggle_lights(Start, End, Lights).

parse_coordinates(Coordinates) ->
    [Start, End] = binary:split(Coordinates, <<" through ">>),
    {parse_coordinate(Start), parse_coordinate(End)}.

parse_coordinate(Coordinate) ->
    [X, Y] = binary:split(Coordinate, <<",">>),
    {binary_to_integer(X), binary_to_integer(Y)}.

turn_on_lights({StartX, StartY}, {EndX, EndY}, Lights) ->
    lists:foldl(fun({X, Y}, AccLights) ->
                    sets:add_element({X, Y}, AccLights)
                end, Lights, coordinates_range(StartX, StartY, EndX, EndY)).

turn_off_lights({StartX, StartY}, {EndX, EndY}, Lights) ->
    lists:foldl(fun({X, Y}, AccLights) ->
                    sets:del_element({X, Y}, AccLights)
                end, Lights, coordinates_range(StartX, StartY, EndX, EndY)).

toggle_lights({StartX, StartY}, {EndX, EndY}, Lights) ->
    lists:foldl(fun({X, Y}, AccLights) ->
                    case sets:is_element({X, Y}, AccLights) of
                        true -> sets:del_element({X, Y}, AccLights);
                        false -> sets:add_element({X, Y}, AccLights)
                    end
                end, Lights, coordinates_range(StartX, StartY, EndX, EndY)).

coordinates_range(StartX, StartY, EndX, EndY) ->
    [{X, Y} || X <- lists:seq(StartX, EndX), Y <- lists:seq(StartY, EndY)].
