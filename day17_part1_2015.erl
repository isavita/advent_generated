-module(task).
-export([call/0]).

count_combinations(Containers, Target, Index) when Target == 0 ->
    1;
count_combinations(Containers, Target, Index) when Target < 0 orelse Index >= length(Containers) ->
    0;
count_combinations(Containers, Target, Index) ->
    count_combinations(Containers, Target - lists:nth(Index + 1, Containers), Index + 1) +
    count_combinations(Containers, Target, Index + 1).

call() ->
    {ok, File} = file:open("input.txt", [read]),
    Containers = read_file(File, []),
    Answer = count_combinations(Containers, 150, 0),
    io:format("~p~n", [Answer]),
    file:close(File).

read_file(File, Acc) ->
    case file:read_line(File) of
        {ok, Data} ->
            [Size | _] = string:tokens(Data, "\n"),
            SizeInt = list_to_integer(Size),
            read_file(File, Acc ++ [SizeInt]);
        eof ->
            Acc
    end.