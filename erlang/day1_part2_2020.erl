-module(task).
-export([call/0]).

call() ->
    {ok, File} = file:read_file("input.txt"),
    Input = string:tokens(binary_to_list(File), "\n"),
    Answer1 = part1(lists:map(fun(X) -> list_to_integer(X) end, Input), 2020),
    Answer2 = part2(lists:map(fun(X) -> list_to_integer(X) end, Input), 2020),
    io:format("~p~n~p~n", [Answer1, Answer2]).

part1(Numbers, Target) ->
    lists:max([A*B || A <- Numbers, B <- Numbers, A + B =:= Target]).

part2(Numbers, Target) ->
    lists:max([A*B*C || A <- Numbers, B <- Numbers, C <- Numbers, A + B + C =:= Target]).