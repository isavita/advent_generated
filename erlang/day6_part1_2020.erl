-module(task).
-export([call/0]).

call() ->
    {ok, File} = file:read_file("input.txt"),
    Groups = string:split(binary_to_list(File), "\n\n", all),
    Answer = lists:sum([length(lists:usort(lists:flatten(string:tokens(Group, "\n")))) || Group <- Groups]),
    io:format("~p~n", [Answer]).