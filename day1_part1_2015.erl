-module(task).
-export([call/0]).

call() ->
    {ok, Binary} = file:read_file("input.txt"),
    Instructions = binary_to_list(Binary),
    Floor = lists:foldl(fun(X, Acc) -> if X == $( -> Acc + 1; true -> Acc - 1 end end, 0, Instructions),
    io:format("~p~n", [Floor]).