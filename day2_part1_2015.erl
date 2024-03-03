-module(task).
-export([call/0]).

call() ->
    {ok, File} = file:read_file("input.txt"),
    Lines = string:split(binary_to_list(File), "\n", all),
    TotalArea = lists:foldl(fun(Present, Acc) ->
        [L, W, H] = string:tokens(Present, "x"),
        LNum = list_to_integer(L),
        WNum = list_to_integer(W),
        HNum = list_to_integer(H),
        Side1 = LNum * WNum,
        Side2 = WNum * HNum,
        Side3 = HNum * LNum,
        MinSide = lists:min([Side1, Side2, Side3]),
        2*LNum*WNum + 2*WNum*HNum + 2*HNum*LNum + MinSide + Acc
    end, 0, Lines),
    io:format("~p~n", [TotalArea]).