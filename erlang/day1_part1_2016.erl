-module(task).
-export([call/0]).

call() ->
    {ok, Data} = file:read_file("input.txt"),
    Instructions = binary:split(Data, <<",">>, [global, trim]),
    {X, Y} = follow_instructions(Instructions, {0, 0}, north),
    Answer = abs(X) + abs(Y),
    io:format("~p~n", [Answer]).

follow_instructions([], Pos, _Direction) ->
    Pos;
follow_instructions([H|T], {X, Y}, Direction) ->
    Instruction = binary_to_list(H),
    [Turn|Rest] = string:trim(Instruction),
    Steps = list_to_integer(Rest),
    {NewDirection, DX, DY} = get_movement(Turn, Direction, Steps),
    follow_instructions(T, {X + DX, Y + DY}, NewDirection).

get_movement($L, north, Steps) -> {west, -Steps, 0};
get_movement($R, north, Steps) -> {east, Steps, 0};
get_movement($L, south, Steps) -> {east, Steps, 0};
get_movement($R, south, Steps) -> {west, -Steps, 0};
get_movement($L, east, Steps) -> {north, 0, Steps};
get_movement($R, east, Steps) -> {south, 0, -Steps};
get_movement($L, west, Steps) -> {south, 0, -Steps};
get_movement($R, west, Steps) -> {north, 0, Steps}.
