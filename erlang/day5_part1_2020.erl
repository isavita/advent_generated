-module(task).
-export([call/0]).

call() ->
    {ok, File} = file:open("input.txt", [read]),
    Data = read_lines(File, []),
    file:close(File),
    Seats = lists:map(fun(X) -> decode_seat(X) end, Data),
    MaxSeatID = lists:max(Seats),
    io:format("~p~n", [MaxSeatID]).

read_lines(File, Acc) ->
    case io:get_line(File, "") of
        eof -> lists:reverse(Acc);
        Line -> read_lines(File, [Line | Acc])
    end.

decode_seat(Code) ->
    RowCode = string:substr(Code, 1, 7),
    ColCode = string:substr(Code, 8, 3),
    Row = decode_binary(RowCode, 0, 127),
    Col = decode_binary(ColCode, 0, 7),
    Row * 8 + Col.

decode_binary([], Min, Max) -> Min;
decode_binary([H | T], Min, Max) ->
    Mid = (Min + Max + 1) div 2,
    case H of
        $F -> decode_binary(T, Min, Mid - 1);
        $B -> decode_binary(T, Mid, Max);
        $L -> decode_binary(T, Min, Mid - 1);
        $R -> decode_binary(T, Mid, Max)
    end.