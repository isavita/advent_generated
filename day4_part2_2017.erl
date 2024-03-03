-module(task).
-export([call/0]).

call() ->
    {ok, Data} = file:read_file("input.txt"),
    Passphrases = string:tokens(binary_to_list(Data), "\n"),
    ValidCount = lists:foldl(fun(Passphrase, Acc) ->
        Words = string:tokens(Passphrase, " "),
        WordSet = lists:foldl(fun(Word, {Set, Valid}) ->
            SortedWord = lists:sort(Word),
            case sets:is_element(SortedWord, Set) of
                true -> {Set, false};
                false -> {sets:add_element(SortedWord, Set), Valid}
            end
        end, {sets:new(), true}, Words),
        case element(2, WordSet) of
            true -> Acc + 1;
            false -> Acc
        end
    end, 0, Passphrases),
    io:format("~p~n", [ValidCount]).