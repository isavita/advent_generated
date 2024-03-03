-module(task).
-export([call/0]).

call() ->
    {ok, Input} = file:read_file("input.txt"),
    StartingNumbers = string:tokens(binary:bin_to_list(Input), ",\n"),
    LastSpoken = dict:new(),
    {_, Answer} = lists:foldl(fun(Turn, {PrevNumber, LastSpokenDict}) ->
        case Turn of
            1 -> {list_to_integer(hd(StartingNumbers)), LastSpokenDict};
            _ when Turn =< 2020 ->
                LastNumber = PrevNumber,
                NextNumber = case dict:find(LastNumber, LastSpokenDict) of
                    {ok, LastTurn} when LastTurn /= Turn-1 -> Turn-1 - LastTurn;
                    _ -> 0
                end,
                NewLastSpokenDict = dict:store(LastNumber, Turn-1, LastSpokenDict),
                {NextNumber, NewLastSpokenDict}
        end
    end, {0, LastSpoken}, lists:seq(1, 2020)),
    io:format("~p~n", [Answer]).