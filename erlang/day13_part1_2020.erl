-module(task).
-export([call/0]).

call() ->
    {ok, File} = file:open("input.txt", [read]),
    {ok, [EarliestDeparture]} = io:fread(File, "", "~d"),
    {ok, [BusIDs]} = io:fread(File, "", "~s"),
    file:close(File),
    
    {EarliestBusID, MinWaitTime} = lists:foldl(
        fun(Id, {EarliestID, MinTime}) ->
            case Id of
                "x" -> {EarliestID, MinTime};
                _ ->
                    BusID = list_to_integer(Id),
                    WaitTime = BusID - (EarliestDeparture rem BusID),
                    case WaitTime < MinTime of
                        true -> {BusID, WaitTime};
                        false -> {EarliestID, MinTime}
                    end
            end
        end,
        {0, EarliestDeparture},
        string:tokens(BusIDs, ",")
    ),
    
    io:format("~p~n", [EarliestBusID * MinWaitTime]).