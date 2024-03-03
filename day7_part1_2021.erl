-module(task).
-export([call/0]).

call() ->
    {ok, File} = file:read_file("input.txt"),
    Input = string:tokens(binary_to_list(File), ",\n"),
    Positions = lists:map(fun(X) -> list_to_integer(X) end, Input),
    MinPos = lists:min(Positions),
    MaxPos = lists:max(Positions),
    Answer = lists:min([lists:sum(lists:map(fun(P) -> abs(P-X) end, Positions)) || X <- lists:seq(MinPos, MaxPos)]),
    io:format("~p~n", [Answer]).