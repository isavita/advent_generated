-module(task).
-export([call/0]).

call() ->
    {ok, Data} = file:read_file("input.txt"),
    Strings = string:tokens(binary_to_list(Data), "\n"),
    Answer = lists:sum([1 || Str <- Strings, is_nice(Str) == true]),
    io:format("~p~n", [Answer]).

is_nice(Str) ->
    Vowels = lists:sum([1 || C <- Str, lists:member(C, "aeiou")]),
    DoubleLetter = check_double_letter(Str),
    NoBadStrings = case lists:member(true, [string:str(Str, Bad) /= 0 || Bad <- ["ab", "cd", "pq", "xy"]]) of
                        true -> false;
                        false -> true
                    end,
    Vowels >= 3 andalso DoubleLetter == true andalso NoBadStrings.

check_double_letter([X, Y | T]) when X == Y -> true;
check_double_letter([_ | T]) -> check_double_letter(T);
check_double_letter(_) -> false.