-module(task).
-export([call/0]).

call() ->
    {ok, Data} = file:read_file("input.txt"),
    Lines = string:split(binary_to_list(Data), "\n", all),
    CalibrationValues = lists:map(fun get_calibration_value/1, Lines),
    Sum = lists:sum(CalibrationValues),
    io:format("~p~n", [Sum]).

get_calibration_value(Line) ->
    Digits = [C || C <- Line, C >= $0, C =< $9],
    case Digits of
        [First | _] = All ->
            Last = lists:last(All),
            FirstDigit = First - $0,
            LastDigit = Last - $0,
            FirstDigit * 10 + LastDigit;
        _ ->
            0
    end.
