-module(task).
-export([call/0]).

call() ->
    {ok, File} = file:open("input.txt", [read]),
    {ok, Numbers} = read_numbers(File, []),
    Answer = find_answer(Numbers),
    io:format("~p~n", [Answer]),
    file:close(File).

read_numbers(File, Acc) ->
    case file:read_line(File) of
        {ok, Line} ->
            case string:strip(Line, right, $\n) of
                "" -> read_numbers(File, Acc);
                NumStr -> 
                    Num = list_to_integer(NumStr),
                    read_numbers(File, [Num | Acc])
            end;
        eof -> {ok, lists:reverse(Acc)}
    end.

find_answer(Numbers) ->
    lists:foreach(
        fun(I) -> 
            lists:foreach(
                fun(J) ->
                    case I + J of
                        2020 -> io:format("~p~n", [I * J]), halt(0);
                        _ -> ok
                    end
                end, Numbers)
        end, Numbers).