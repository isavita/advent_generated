-module(task).
-export([call/0]).

call() ->
    {StartA, StartB} = read_input("input.txt"),
    Count = count_matching_pairs(StartA, StartB, 40000000),
    io:format("~p~n", [Count]).

read_input(FileName) ->
    {ok, File} = file:read_file(FileName),
    [StartA, StartB] = lists:map(fun(X) -> list_to_integer(X) end, string:tokens(binary_to_list(File), "\r\n")),
    {StartA, StartB}.

count_matching_pairs(StartA, StartB, 0) ->
    0;
count_matching_pairs(StartA, StartB, N) ->
    NextA = generate_next(StartA, 16807),
    NextB = generate_next(StartB, 48271),
    Count = if NextA band 65535 == NextB band 65535 -> 1; true -> 0 end,
    count_matching_pairs(NextA, NextB, N-1) + Count.

generate_next(Prev, Factor) ->
    (Prev * Factor rem 2147483647).