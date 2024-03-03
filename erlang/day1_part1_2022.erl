-module(task).
-export([call/0]).

call() ->
    {ok, File} = file:open("input.txt", [read]),
    {ok, Answer} = find_max_calories(File, 0, 0),
    io:format("~p~n", [Answer]),
    file:close(File).

find_max_calories(File, MaxCalories, CurrentCalories) ->
    case file:read_line(File) of
        {ok, Line} ->
            case Line of
                "\n" ->
                    if CurrentCalories > MaxCalories ->
                        find_max_calories(File, CurrentCalories, 0);
                    true ->
                        find_max_calories(File, MaxCalories, 0)
                    end;
                _ ->
                    case list_to_integer(string:strip(Line, right, $\n)) of
                        {error, _} -> find_max_calories(File, MaxCalories, CurrentCalories);
                        Calories -> find_max_calories(File, MaxCalories, CurrentCalories + Calories)
                    end
            end;
        eof ->
            if CurrentCalories > MaxCalories ->
                {ok, CurrentCalories};
            true ->
                {ok, MaxCalories}
            end
    end.