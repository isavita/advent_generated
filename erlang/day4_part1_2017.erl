-module(task).
-export([call/0]).

call() ->
    {ok, File} = file:read_file("input.txt"),
    Passphrases = string:tokens(binary_to_list(File), "\n"),
    ValidPassphrases = lists:filter(fun(P) -> is_valid_passphrase(P) end, Passphrases),
    io:format("~p~n", [length(ValidPassphrases)]).

is_valid_passphrase(Passphrase) ->
    Words = string:tokens(Passphrase, " "),
    length(Words) =:= length(lists:usort(Words)).