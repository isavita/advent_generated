-module(task).
-export([call/0]).

call() ->
    {ok, File} = file:open("input.txt", [read]),
    {ok, Count} = count_inversions(File, 0, 0),
    io:format("~p~n", [Count]),
    file:close(File).

count_inversions(File, Prev, Count) ->
    case io:get_line(File, "") of
        eof ->
            {ok, Count};
        {error, Reason} ->
            {error, Reason};
        Line ->
            [NumStr | _] = string:tokens(Line, "\n"),
            Current = list_to_integer(NumStr),
            if
                Prev /= 0 andalso Current > Prev ->
                    count_inversions(File, Current, Count + 1);
                true ->
                    count_inversions(File, Current, Count)
            end
    end.