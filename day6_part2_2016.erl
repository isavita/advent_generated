-module(task).
-export([call/0]).

call() ->
    {ok, Input} = file:read_file("input.txt"),
    Lines = binary:split(Input, <<"\n">>, [global, trim]),
    Answer = solve(Lines),
    io:format("~s~n", [Answer]).

solve(Lines) ->
    Len = length(binary_to_list(hd(Lines))),
    Freqs = [{Pos, freq(Pos, Lines)} || Pos <- lists:seq(1, Len)],
    list_to_binary([Least || {_, Least} <- Freqs]).

freq(Pos, Lines) ->
    Chars = [binary:at(Line, Pos-1) || Line <- Lines],
    Counts = lists:foldl(fun(C, Acc) -> maps:update_with(C, fun(V) -> V + 1 end, 1, Acc) end, #{}, Chars),
    {_, Min} = lists:min([{Count, Char} || {Char, Count} <- maps:to_list(Counts)]),
    Min.
