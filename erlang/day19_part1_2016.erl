-module(task).
-export([call/0]).

call() ->
    {ok, Data} = file:read_file("input.txt"),
    TotalElves = list_to_integer(binary_to_list(Data)),
    Winner = find_winning_elf(TotalElves),
    io:format("~p~n", [Winner]).

find_winning_elf(TotalElves) ->
    HighestPowerOfTwo = highest_power_of_two(TotalElves),
    (TotalElves - HighestPowerOfTwo) * 2 + 1.

highest_power_of_two(N) when N > 0 ->
    highest_power_of_two(N, 1).

highest_power_of_two(N, Power) when Power * 2 =< N ->
    highest_power_of_two(N, Power * 2);
highest_power_of_two(_, Power) ->
    Power.