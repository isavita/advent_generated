-module(task).
-export([call/0]).

call() ->
    {ok, Binary} = file:read_file("input.txt"),
    Data = binary_to_list(Binary),
    Answer = find_marker(Data, 0),
    io:format("~p~n", [Answer]).

find_marker([A, B, C, D | _], Index) when A =/= B, A =/= C, A =/= D, B =/= C, B =/= D, C =/= D ->
    Index + 4;
find_marker([_ | T], Index) ->
    find_marker(T, Index + 1);
find_marker(_, _) ->
    0.