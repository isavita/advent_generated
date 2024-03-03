-module(task).
-export([call/0]).

call() ->
    {HPos, Depth} = process_file("input.txt", {0, 0}),
    Answer = HPos * Depth,
    io:format("~p~n", [Answer]).

process_file(FileName, {HPos, Depth}) ->
    {ok, File} = file:read_file(FileName),
    Instructions = string:tokens(binary_to_list(File), "\n"),
    process_instructions(Instructions, {HPos, Depth}).

process_instructions([], {HPos, Depth}) ->
    {HPos, Depth};
process_instructions([Instruction|Rest], {HPos, Depth}) ->
    [Action, ValueStr] = string:tokens(Instruction, " "),
    Value = list_to_integer(ValueStr),
    case Action of
        "forward" -> process_instructions(Rest, {HPos + Value, Depth});
        "down" -> process_instructions(Rest, {HPos, Depth + Value});
        "up" -> process_instructions(Rest, {HPos, Depth - Value})
    end.