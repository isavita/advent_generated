-module(task).
-export([call/0]).

call() ->
    {ok, File} = file:read_file("input.txt"),
    [CardPublicKeyStr, DoorPublicKeyStr] = string:tokens(binary_to_list(File), "\n"),
    CardPublicKey = list_to_integer(CardPublicKeyStr),
    DoorPublicKey = list_to_integer(DoorPublicKeyStr),
    CardLoopSize = find_loop_size(CardPublicKey, 1, 0),
    EncryptionKey = transform(DoorPublicKey, CardLoopSize),
    io:format("~p~n", [EncryptionKey]).

find_loop_size(PublicKey, Value, LoopSize) when Value =:= PublicKey ->
    LoopSize;
find_loop_size(PublicKey, Value, LoopSize) ->
    NewValue = (Value * 7) rem 20201227,
    find_loop_size(PublicKey, NewValue, LoopSize + 1).

transform(SubjectNumber, LoopSize) ->
    lists:foldl(fun(_, Acc) -> (Acc * SubjectNumber) rem 20201227 end, 1, lists:seq(1, LoopSize)).