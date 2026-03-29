-module(task).
-export([call/0]).

call() ->
    {ok, Bin} = file:read_file("input.txt"),
    RawLines = string:split(binary_to_list(Bin), "\n", all),
    Lines = [string:trim(Line) || Line <- RawLines, string:trim(Line) =/= ""],
    io:format("~p~n", [sum_badges(Lines, 0)]).

sum_badges([], Acc) ->
    Acc;
sum_badges([A, B, C | Rest], Acc) ->
    Badge = find_badge(A, B, C),
    sum_badges(Rest, Acc + priority(Badge)).

find_badge(A, B, C) ->
    SetB = maps:from_list([{Ch, true} || Ch <- B]),
    SetC = maps:from_list([{Ch, true} || Ch <- C]),
    find_common(A, SetB, SetC).

find_common([Ch | Rest], SetB, SetC) ->
    case maps:is_key(Ch, SetB) andalso maps:is_key(Ch, SetC) of
        true -> Ch;
        false -> find_common(Rest, SetB, SetC)
    end.

priority(Ch) when Ch >= $a, Ch =< $z ->
    Ch - $a + 1;
priority(Ch) when Ch >= $A, Ch =< $Z ->
    Ch - $A + 27.
