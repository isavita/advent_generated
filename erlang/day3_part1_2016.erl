-module(task).
-export([call/0]).

call() ->
    {ok, File} = file:read_file("input.txt"),
    Triangles = string:tokens(binary:bin_to_list(File), "\n"),
    Answer = lists:foldl(fun(Triangle, Acc) -> Acc + case check_triangle(lists:map(fun(X) -> list_to_integer(X) end, string:tokens(Triangle, " \t\n")), 0) of true -> 1; false -> 0 end end, 0, Triangles),
    io:format("~p~n", [Answer]).

check_triangle([A, B, C], Acc) when A + B > C, A + C > B, B + C > A -> true;
check_triangle(_, Acc) -> false.