-module(task).
-export([call/0]).

call() ->
    {ok, Data} = file:read_file("input.txt"),
    Instructions = binary_to_list(Data),
    Answer = find_basement(Instructions, 0, 1),
    io:format("~p", [Answer]).

find_basement([], _Floor, _Position) ->
    not_found; % In case the basement is never reached
find_basement([H|T], Floor, Position) ->
    NewFloor = case H of
        $( -> Floor + 1; % Increase floor if '('
        $) -> Floor - 1  % Decrease floor if ')'
    end,
    if
        NewFloor == -1 -> Position; % Return the position if basement is reached
        true -> find_basement(T, NewFloor, Position + 1) % Otherwise, continue with the next character
    end.
