-module(task).
-export([call/0]).

call() ->
    {ok, Binary} = file:read_file("input.txt"),
    Lines = binary:split(Binary, <<"\n">>, [global, trim_all]),
    Answer = count_overlaps(Lines, #{}, 0),
    io:format("~p~n", [Answer]).

count_overlaps([], Map, Count) ->
    maps:fold(fun(_, V, AccCount) when V >= 2 -> AccCount + 1;
                 (_, _, AccCount) -> AccCount
              end, Count, Map);
count_overlaps([Line | Rest], Map, Count) ->
    {X1, Y1, X2, Y2} = parse_line(Line),
    NewMap = mark_points(Map, X1, Y1, X2, Y2),
    count_overlaps(Rest, NewMap, Count).

parse_line(Line) ->
    [Part1, Part2] = binary:split(Line, <<" -> ">>),
    [X1, Y1] = [binary_to_integer(I) || I <- binary:split(Part1, <<",">>)],
    [X2, Y2] = [binary_to_integer(I) || I <- binary:split(Part2, <<",">>)],
    {X1, Y1, X2, Y2}.

mark_points(Map, X1, Y1, X2, Y2) when X1 == X2 ->
    Ys = lists:seq(min(Y1, Y2), max(Y1, Y2)),
    lists:foldl(fun(Y, AccMap) -> maps:update_with({{X1, Y}, 0}, fun(V) -> V + 1 end, 1, AccMap) end, Map, Ys);
mark_points(Map, X1, Y1, X2, Y2) when Y1 == Y2 ->
    Xs = lists:seq(min(X1, X2), max(X1, X2)),
    lists:foldl(fun(X, AccMap) -> maps:update_with({{X, Y1}, 0}, fun(V) -> V + 1 end, 1, AccMap) end, Map, Xs);
mark_points(Map, X1, Y1, X2, Y2) ->
    DeltaX = if X1 < X2 -> 1; true -> -1 end,
    DeltaY = if Y1 < Y2 -> 1; true -> -1 end,
    mark_diagonal(Map, X1, Y1, X2, Y2, DeltaX, DeltaY).

mark_diagonal(Map, X, Y, X2, Y2, _, _) when X == X2, Y == Y2 ->
    maps:update_with({{X, Y}, 0}, fun(V) -> V + 1 end, 1, Map);
mark_diagonal(Map, X, Y, X2, Y2, DeltaX, DeltaY) ->
    NewMap = maps:update_with({{X, Y}, 0}, fun(V) -> V + 1 end, 1, Map),
    mark_diagonal(NewMap, X + DeltaX, Y + DeltaY, X2, Y2, DeltaX, DeltaY).
