-module(task).
-export([call/0]).

call() ->
    {ok, Data} = file:read_file("input.txt"),
    Digits = binary_to_list(Data),
    % Ensure we remove any newline character at the end of the input
    CleanDigits = lists:filter(fun(X) -> X =/= $\n end, Digits),
    Answer = calculate_sum(CleanDigits),
    io:format("~p~n", [Answer]).

calculate_sum(Digits) ->
    % Append the first element to the end to make the list circular
    CircularDigits = Digits ++ [hd(Digits)],
    calculate_sum(CircularDigits, 0).

calculate_sum([H, Next|T], Acc) when H == Next ->
    calculate_sum([Next|T], Acc + (H - $0)); % $0 is the char code for '0', subtracting it converts char to int
calculate_sum([_H, Next|T], Acc) ->
    calculate_sum([Next|T], Acc);
calculate_sum([_], Acc) ->
    Acc.
