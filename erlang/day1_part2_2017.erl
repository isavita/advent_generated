-module(task).
-export([call/0]).

call() ->
    {ok, Data} = file:read_file("input.txt"),
    Input = string:trim(binary_to_list(Data)),
    Halfway = length(Input) div 2,
    Sum = lists:sum([digit_sum(Input, I, Halfway) || I <- lists:seq(0, length(Input) - 1)]),
    io:format("~p~n", [Sum]).

digit_sum(Input, I, Halfway) ->
    Next = (I + Halfway) rem length(Input),
    case lists:nth(I + 1, Input) == lists:nth(Next + 1, Input) of
        true -> lists:nth(I + 1, Input) - $0;
        false -> 0
    end.