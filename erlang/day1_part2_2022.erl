-module(task).
-export([call/0]).

call() ->
    {ok, Bin} = file:read_file("input.txt"),
    Lines = string:split(binary_to_list(Bin), "\n", all),
    TopThree = process_lines(Lines, 0, [0, 0, 0]),
    io:format("~p~n", [lists:sum(TopThree)]).

process_lines([], Current, TopThree) ->
    update_top_three(Current, TopThree);
process_lines([Line | Rest], Current, TopThree) ->
    case string:trim(Line) of
        "" ->
            process_lines(Rest, 0, update_top_three(Current, TopThree));
        NumberText ->
            Calories = list_to_integer(NumberText),
            process_lines(Rest, Current + Calories, TopThree)
    end.

update_top_three(Value, TopThree) ->
    lists:sublist(lists:reverse(lists:sort([Value | TopThree])), 3).
