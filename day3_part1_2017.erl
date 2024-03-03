-module(task).
-export([call/0]).

call() ->
    {ok, Binary} = file:read_file("input.txt"),
    Input = list_to_integer(binary:bin_to_list(Binary)),
    Side = round(math:sqrt(Input)),
    Center = (Side - 1) / 2,
    Answer = abs(((Input - 1) rem (Side - 1)) - Center) + Center,
    io:format("~p~n", [Answer]).