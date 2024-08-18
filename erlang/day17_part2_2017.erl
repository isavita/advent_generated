-module(task).
-export([call/0]).

call() ->
    {ok, Data} = file:read_file("input.txt"),
    Steps = list_to_integer(binary_to_list(Data)),
    ValueAfterZero = find_value_after_zero(Steps, 50000000, 0, 0),
    io:format("~p~n", [ValueAfterZero]).

find_value_after_zero(Steps, N, CurrentPos, ValueAfterZero) ->
    find_value_after_zero(Steps, N, CurrentPos, ValueAfterZero, 1).

find_value_after_zero(_, 0, _, ValueAfterZero, _) -> ValueAfterZero;
find_value_after_zero(Steps, N, CurrentPos, ValueAfterZero, I) ->
    NewPos = (CurrentPos + Steps) rem I,
    NewValueAfterZero = if NewPos == 0 -> I; true -> ValueAfterZero end,
    find_value_after_zero(Steps, N - 1, NewPos + 1, NewValueAfterZero, I + 1).