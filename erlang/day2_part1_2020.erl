-module(task).
-export([call/0]).

call() ->
    {ok, File} = file:read_file("input.txt"),
    Lines = string:tokens(binary_to_list(File), "\n"),
    Answer = lists:foldl(fun(X, Acc) -> case check_password(X) of true -> Acc + 1; false -> Acc end end, 0, Lines),
    io:format("~p~n", [Answer]).

check_password(Line) ->
    [MinStr, MaxStr, CharStr, Password] = string:tokens(Line, " :-\r"),
    Min = list_to_integer(MinStr),
    Max = list_to_integer(MaxStr),
    Char = hd(CharStr),
    Count = lists:foldl(fun(X, Acc) -> if X == Char -> Acc + 1; true -> Acc end end, 0, Password),
    Count >= Min andalso Count =< Max.