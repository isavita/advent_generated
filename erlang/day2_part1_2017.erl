-module(task).
-export([call/0]).

call() ->
    {ok, File} = file:read_file("input.txt"),
    Rows = string:tokens(binary_to_list(File), "\n"),
    Checksum = lists:foldl(fun(Row, Acc) -> Acc + row_checksum(lists:map(fun(X) -> list_to_integer(X) end, string:tokens(Row, " \t\n")), 0) end, 0, Rows),
    io:format("~p~n", [Checksum]).

row_checksum(Row, Acc) ->
    Max = lists:max(Row),
    Min = lists:min(Row),
    Max - Min + Acc.