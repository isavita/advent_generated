-module(task).
-export([call/0]).

call() ->
    {ok, Bin} = file:read_file("input.txt"),
    RawLines = string:split(binary_to_list(Bin), "\n", all),
    Lines = [string:trim(Line) || Line <- RawLines, string:trim(Line) =/= ""],
    Score = total_score(Lines, 0),
    io:format("~p~n", [Score]).

total_score([], Acc) ->
    Acc;
total_score([Line | Rest], Acc) ->
    [OppText, OutcomeText] = string:tokens(Line, " "),
    Opponent = decode_shape(OppText),
    OutcomeScore = decode_outcome(OutcomeText),
    MyShape = choose_shape(Opponent, OutcomeText),
    total_score(Rest, Acc + OutcomeScore + MyShape).

decode_shape("A") -> 1;
decode_shape("B") -> 2;
decode_shape("C") -> 3.

decode_outcome("X") -> 0;
decode_outcome("Y") -> 3;
decode_outcome("Z") -> 6.

choose_shape(Opponent, "Y") -> Opponent;
choose_shape(1, "X") -> 3;
choose_shape(2, "X") -> 1;
choose_shape(3, "X") -> 2;
choose_shape(1, "Z") -> 2;
choose_shape(2, "Z") -> 3;
choose_shape(3, "Z") -> 1.
