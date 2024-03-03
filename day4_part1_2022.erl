-module(task).
-export([call/0, check_pair/2]).

call() ->
    {ok, File} = file:read_file("input.txt"),
    Input = binary_to_list(File),
    Pairs = string:tokens(Input, "\n"),
    Answer = lists:foldl(fun check_pair/2, 0, Pairs),
    io:format("~p~n", [Answer]).

check_pair(Pair, Count) ->
    [Range1, Range2] = string:tokens(Pair, ", "),
    [Start1, End1] = lists:map(fun(X) -> list_to_integer(X) end, string:tokens(Range1, "-")),
    [Start2, End2] = lists:map(fun(X) -> list_to_integer(X) end, string:tokens(Range2, "-")),
    if
        (Start1 =< Start2) and (End1 >= End2) -> Count + 1;
        (Start2 =< Start1) and (End2 >= End1) -> Count + 1;
        true -> Count
    end.