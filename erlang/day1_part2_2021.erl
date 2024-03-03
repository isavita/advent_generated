-module(task).
-export([call/0]).

read_file(FileName) ->
    {ok, Binary} = file:read_file(FileName),
    Lines = binary:split(Binary, <<"\n">>, [global, trim]),
    lists:map(fun(X) -> list_to_integer(binary:bin_to_list(X)) end, Lines).

count_increases([_]) -> 0;
count_increases([H1,H2|T]) when H2 > H1 -> 1 + count_increases([H2|T]);
count_increases([H1,H2|T]) -> count_increases([H2|T]).

sliding_window_sums([_,_,_]=L, Acc) -> {Acc, L};
sliding_window_sums([H1,H2,H3|T], {Count, PrevSum}) ->
    Sum = H1 + H2 + H3,
    case Sum of
        _ when Count == 0 -> sliding_window_sums([H2,H3|T], {1, Sum});
        _ when Sum > PrevSum -> sliding_window_sums([H2,H3|T], {Count+1, Sum});
        _ -> sliding_window_sums([H2,H3|T], {Count, Sum})
    end.

call() ->
    Depths = read_file("input.txt"),
    Answer1 = count_increases(Depths),
    {Answer2, _} = sliding_window_sums(Depths, {0, 0}),
    io:format("~p~n~p~n", [Answer1, Answer2]).