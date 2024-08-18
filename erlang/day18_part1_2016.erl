-module(task).
-export([call/0]).

call() ->
    {ok, Data} = file:read_file("input.txt"),
    FirstRow = binary_to_list(Data),
    SafeTilesCount = count_safe_tiles(FirstRow, 40),
    io:format("~p~n", [SafeTilesCount]).

count_safe_tiles(FirstRow, TotalRows) ->
    SafeCount = count_char(FirstRow, $.), 
    count_rows(FirstRow, TotalRows - 1, SafeCount).

count_rows(CurrentRow, 0, SafeCount) -> SafeCount;
count_rows(CurrentRow, RemainingRows, SafeCount) ->
    NextRow = generate_next_row(CurrentRow),
    NewSafeCount = SafeCount + count_char(NextRow, $.),
    count_rows(NextRow, RemainingRows - 1, NewSafeCount).

generate_next_row(Row) ->
    lists:map(fun(Index) -> 
        case is_trap(Index - 1, Index, Index + 1, Row) of
            true -> $^;
            false -> $. 
        end
    end, lists:seq(0, length(Row) - 1)).

is_trap(Left, Center, Right, Row) ->
    L = safe_if_out_of_bounds(Left, Row),
    C = lists:nth(Center + 1, Row),
    R = safe_if_out_of_bounds(Right, Row),
    (L == $^ andalso C == $^ andalso R == $.) orelse
    (C == $^ andalso R == $^ andalso L == $.) orelse
    (L == $^ andalso C == $. andalso R == $.) orelse
    (R == $^ andalso C == $. andalso L == $.).

safe_if_out_of_bounds(Index, Row) ->
    case Index of
        I when I < 0 -> $.;
        I when I >= length(Row) -> $.;
        I -> lists:nth(I + 1, Row)
    end.

count_char(Str, Char) ->
    lists:sum([1 || C <- Str, C == Char]).