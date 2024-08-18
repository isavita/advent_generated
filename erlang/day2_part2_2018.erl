-module(task).
-export([call/0]).

call() ->
    {ok, Data} = file:read_file("input.txt"),
    Lines = string:split(binary_to_list(Data), "\n", all),
    find_common(Lines).

find_common(Lines) ->
    lists:foreach(fun(X) -> 
        lists:foreach(fun(Y) -> 
            case compare_strings(X, Y) of
                {ok, Common} -> io:format("~s~n", [Common]);
                _ -> ok
            end
        end, Lines)
    end, Lines).

compare_strings(Str1, Str2) ->
    {Diff, Common} = lists:foldl(fun({C1, C2}, {D, C}) ->
        if
            C1 == C2 -> {D, [C1 | C]};
            true -> {D + 1, C}
        end
    end, {0, []}, lists:zip(Str1, Str2)),
    if
        Diff == 1 -> {ok, lists:reverse(Common)};
        true -> error
    end.