-module(task).
-export([call/0]).

call() ->
    {ok, File} = file:read_file("input.txt"),
    Input = binary_to_list(File),
    Frequencies = string:split(Input, "\n", all),
    Result = lists:sum(lists:map(fun(X) -> list_to_integer(string:strip(X)) end, Frequencies)),
    io:format("~p~n", [Result]).