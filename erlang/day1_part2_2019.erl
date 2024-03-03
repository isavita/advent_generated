-module(task).
-export([call/0]).

call() ->
    File = "input.txt",
    {ok, Data} = file:read_file(File),
    Numbers = string:tokens(binary_to_list(Data), "\n"),
    Fuel = lists:foldl(fun(X, Acc) -> calculate_fuel(list_to_integer(X)) + Acc end, 0, Numbers),
    io:format("~p~n", [Fuel]).

calculate_fuel(Mass) when Mass =< 0 ->
    0;
calculate_fuel(Mass) ->
    Fuel = (Mass div 3) - 2,
    case Fuel of
        F when F =< 0 -> 0;
        F -> F + calculate_fuel(F)
    end.