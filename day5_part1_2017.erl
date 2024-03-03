-module(task).
-export([call/0]).

parse_input() ->
    {ok, File} = file:read_file("input.txt"),
    [list_to_integer(X) || X <- string:split(binary_to_list(File), "\n",all)].

jump(Instructions, Index, Steps) when Index < 1 orelse Index > length(Instructions) ->
    Steps;
jump(Instructions, Index, Steps) ->
    NextIndex = Index + lists:nth(Index, Instructions),
    NewInstructions = lists:sublist(Instructions, Index-1) ++ [lists:nth(Index, Instructions) + 1] ++ lists:nthtail(Index, Instructions),
    jump(NewInstructions, NextIndex, Steps + 1).

call() ->
    Instructions = parse_input(),
    io:format("~p~n", [jump(Instructions, 1, 0)]).