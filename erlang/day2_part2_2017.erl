-module(task).
-export([call/0]).

call() ->
    {ok, Data} = file:read_file("input.txt"),
    Lines = string:split(binary_to_list(Data), "\n", all),
    Sum = lists:sum(lists:map(fun line_sum/1, Lines)),
    io:format("~p~n", [Sum]).

line_sum(Line) ->
    Numbers = [list_to_integer(N) || N <- string:tokens(Line, " \t")],
    lists:sum([Num1 div Num2 || Num1 <- Numbers, Num2 <- Numbers, Num1 rem Num2 == 0, Num1 /= Num2]).