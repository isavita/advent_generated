-module(task).
-export([call/0]).

call() ->
    Input = "input.txt",
    {ok, File} = file:read_file(Input),
    Lines = string:tokens(binary_to_list(File), "\n"),
    TotalScore = calculate_score(Lines, 0),
    io:format("~p~n", [TotalScore]).

calculate_score([], Score) -> Score;
calculate_score([Line | Rest], Score) ->
    [Opponent, Response] = string:tokens(Line, " "),
    OpponentScore = case Opponent of
                        "A" -> 1;
                        "B" -> 2;
                        "C" -> 3
                    end,
    ResponseScore = case Response of
                        "X" -> 1;
                        "Y" -> 2;
                        "Z" -> 3
                    end,
    Outcome = case OpponentScore of
                1 when ResponseScore == 2 -> 6;
                2 when ResponseScore == 3 -> 6;
                3 when ResponseScore == 1 -> 6;
                _ when OpponentScore == ResponseScore -> 3;
                _ -> 0
              end,
    calculate_score(Rest, Score + ResponseScore + Outcome).