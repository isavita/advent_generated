-module(task).
-export([call/0]).

call() ->
    {ok, File} = file:read_file("input.txt"),
    Lines = string:tokens(binary_to_list(File), "\n"),
    Answer1 = count_trees(Lines, 3, 1),
    Answer2 = count_trees(Lines, 1, 1) * Answer1 * count_trees(Lines, 5, 1) * count_trees(Lines, 7, 1) * count_trees(Lines, 1, 2),
    io:format("~p~n", [Answer1]),
    io:format("~p~n", [Answer2]).

count_trees(Lines, Right, Down) ->
    count_trees(Lines, Right, Down, 0, 0).

count_trees([Line|Rest], Right, Down, Pos, Trees) when Pos rem Down =:= 0 ->
    NewPos = (Pos div Down * Right) rem length(Line),
    NewTrees = Trees + case lists:nth(NewPos + 1, Line) of
                          $# -> 1;
                          _ -> 0
                      end,
    count_trees(Rest, Right, Down, Pos + 1, NewTrees);
count_trees([_|Rest], Right, Down, Pos, Trees) ->
    count_trees(Rest, Right, Down, Pos + 1, Trees);
count_trees([], _, _, _, Trees) ->
    Trees.