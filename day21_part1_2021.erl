-module(task).
-export([call/0]).

call() ->
    {ok, Data} = file:read_file("input.txt"),
    Lines = string:tokens(binary_to_list(Data), "\n"),
    Player1Start = list_to_integer(string:substr(lists:nth(1, Lines), 29, 3)),
    Player2Start = list_to_integer(string:substr(lists:nth(2, Lines), 29, 3)),
    Player1Pos = Player1Start,
    Player2Pos = Player2Start,
    Player1Score = 0,
    Player2Score = 0,
    DieRoll = 1,
    RollCount = 0,
    play(Player1Pos, Player2Pos, Player1Score, Player2Score, DieRoll, RollCount).

play(Player1Pos, Player2Pos, Player1Score, Player2Score, DieRoll, RollCount) ->
    Rolls1 = DieRoll rem 100 + (DieRoll+1) rem 100 + (DieRoll+2) rem 100,
    RollCount1 = RollCount + 3,
    DieRoll1 = DieRoll + 3,
    Player1Pos1 = (Player1Pos + Rolls1 - 1) rem 10 + 1,
    Player1Score1 = Player1Score + Player1Pos1,
    case Player1Score1 >= 1000 of
        true -> io:format("Result: ~p~n", [Player2Score*RollCount1]);
        false -> Rolls2 = DieRoll1 rem 100 + (DieRoll1+1) rem 100 + (DieRoll1+2) rem 100,
                  RollCount2 = RollCount1 + 3,
                  DieRoll2 = DieRoll1 + 3,
                  Player2Pos1 = (Player2Pos + Rolls2 - 1) rem 10 + 1,
                  Player2Score1 = Player2Score + Player2Pos1,
                  case Player2Score1 >= 1000 of
                      true -> io:format("Result: ~p~n", [Player1Score1 * RollCount2]);
                      false -> play(Player1Pos1, Player2Pos1, Player1Score1, Player2Score1, DieRoll2, RollCount2)
                  end
    end.