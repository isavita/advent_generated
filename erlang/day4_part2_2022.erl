-module(task).
-export([call/0]).

parse_line(Line) ->
    [Start1, End1, Start2, End2] = [list_to_integer(X) || X <- string:tokens(Line, "-, ")],
    {Start1, End1, Start2, End2}.

overlap({Start1, End1, Start2, End2}) ->
    max(0, min(End1, End2) - max(Start1, Start2) + 1).

part1() ->
    {ok, File} = file:read_file("input.txt"),
    Lines = string:split(binary_to_list(File), "\n", all),
    Pairs = [parse_line(Line) || Line <- Lines],
    Count = length([true || {Start1, End1, Start2, End2} <- Pairs, overlap({Start1, End1, Start2, End2}) > 0]),
    Count.

part2() ->
    {ok, File} = file:read_file("input.txt"),
    Lines = string:split(binary_to_list(File), "\n", all),
    Pairs = [parse_line(Line) || Line <- Lines],
    Count = length([true || {Start1, End1, Start2, End2} <- Pairs, overlap({Start1, End1, Start2, End2}) > 0]),
    Count.

call() ->
    Part1 = part1(),
    Part2 = part2(),
    io:format("Part 1: ~p~nPart 2: ~p~n", [Part1, Part2]).