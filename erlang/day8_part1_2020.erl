-module(task).
-export([call/0]).

call() ->
    {ok, Data} = file:read_file("input.txt"),
    Instructions = string:split(binary_to_list(Data), "\n", all),
    {Accumulator, _} = execute_boot_code(Instructions, 0, 0, []),
    io:format("~p~n", [Accumulator]).

execute_boot_code([], Accumulator, _, _) -> {Accumulator, true};
execute_boot_code(Instructions, Accumulator, Current, Visited) when Current < length(Instructions) ->
    case lists:member(Current, Visited) of
        true -> {Accumulator, true};
        false ->
            OpArg = lists:nth(Current + 1, Instructions),
            [Op | ArgStr] = string:split(OpArg, " "),
            Arg = list_to_integer(hd(ArgStr)),
            case Op of
                "acc" -> execute_boot_code(Instructions, Accumulator + Arg, Current + 1, [Current | Visited]);
                "jmp" -> execute_boot_code(Instructions, Accumulator, Current + Arg, [Current | Visited]);
                "nop" -> execute_boot_code(Instructions, Accumulator, Current + 1, [Current | Visited])
            end
    end.