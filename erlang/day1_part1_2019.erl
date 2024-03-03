-module(task).
-export([call/0]).

call() ->
    {ok, Input} = file:read_file("input.txt"),
    Lines = string:tokens(binary_to_list(Input), "\n"),
    TotalFuel = lists:sum([((list_to_integer(X) div 3) - 2) || X <- Lines]),
    io:format("~p~n", [TotalFuel]).