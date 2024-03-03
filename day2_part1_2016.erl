-module(task).
-export([call/0]).

call() ->
    {ok, File} = file:read_file("input.txt"),
    Instructions = binary:split(File, <<"\n">>, [global, trim]),
    Code = getCode(Instructions, 5),
    io:format("~s~n", [Code]).

getCode([], Button) ->
    "";
getCode([Instruction | Rest], Button) ->
    NewButton = getNextButton(Button, binary_to_list(Instruction)),
    [integer_to_list(NewButton)] ++ getCode(Rest, NewButton).

getNextButton(Current, []) -> Current;
getNextButton(Current, [H | T]) ->
    case {Current, H} of
        {1, $U} -> getNextButton(1, T);
        {1, $L} -> getNextButton(1, T);
        {1, $R} -> getNextButton(2, T);
        {1, $D} -> getNextButton(4, T);
        {2, $U} -> getNextButton(2, T);
        {2, $L} -> getNextButton(1, T);
        {2, $R} -> getNextButton(3, T);
        {2, $D} -> getNextButton(5, T);
        {3, $U} -> getNextButton(3, T);
        {3, $L} -> getNextButton(2, T);
        {3, $R} -> getNextButton(3, T);
        {3, $D} -> getNextButton(6, T);
        {4, $U} -> getNextButton(1, T);
        {4, $L} -> getNextButton(4, T);
        {4, $R} -> getNextButton(5, T);
        {4, $D} -> getNextButton(7, T);
        {5, $U} -> getNextButton(2, T);
        {5, $L} -> getNextButton(4, T);
        {5, $R} -> getNextButton(6, T);
        {5, $D} -> getNextButton(8, T);
        {6, $U} -> getNextButton(3, T);
        {6, $L} -> getNextButton(5, T);
        {6, $R} -> getNextButton(6, T);
        {6, $D} -> getNextButton(9, T);
        {7, $U} -> getNextButton(4, T);
        {7, $L} -> getNextButton(7, T);
        {7, $R} -> getNextButton(8, T);
        {7, $D} -> getNextButton(7, T);
        {8, $U} -> getNextButton(5, T);
        {8, $L} -> getNextButton(7, T);
        {8, $R} -> getNextButton(9, T);
        {8, $D} -> getNextButton(8, T);
        {9, $U} -> getNextButton(6, T);
        {9, $L} -> getNextButton(8, T);
        {9, $R} -> getNextButton(9, T);
        {9, $D} -> getNextButton(9, T)
    end.