-module(task).
-export([call/0]).

call() ->
    {ok, Input} = file:read_file("input.txt"),
    Program = parse_input(binary_to_list(Input)),
    Answer = find_inputs(Program),
    io:format("~p~n", [Answer]).

parse_input(Input) ->
    [list_to_integer(X) || X <- string:tokens(Input, ",\n")].

find_inputs(Program) ->
    find_inputs(Program, 0, 99).

find_inputs(Program, Noun, Verb) when Noun =< 99 ->
    RestoredProgram = lists:sublist(Program, 1) ++ [Noun, Verb] ++ lists:nthtail(3, Program),
    case run_program(RestoredProgram) of
        19690720 ->
            100 * Noun + Verb;
        _ ->
            if
                Verb < 99 ->
                    find_inputs(Program, Noun, Verb + 1);
                true ->
                    find_inputs(Program, Noun + 1, 0)
            end
    end.

run_program(Program) ->
    run_program(Program, 0).

run_program(Program, Position) ->
    case lists:nth(Position + 1, Program) of
        99 ->
            lists:nth(1, Program);
        1 ->
            Pos1 = lists:nth(Position + 2, Program),
            Pos2 = lists:nth(Position + 3, Program),
            Pos3 = lists:nth(Position + 4, Program),
            Value1 = lists:nth(Pos1 + 1, Program),
            Value2 = lists:nth(Pos2 + 1, Program),
            Result = Value1 + Value2,
            UpdatedProgram = lists:sublist(Program, Pos3) ++ [Result] ++ lists:nthtail(Pos3 + 1, Program),
            run_program(UpdatedProgram, Position + 4);
        2 ->
            Pos1 = lists:nth(Position + 2, Program),
            Pos2 = lists:nth(Position + 3, Program),
            Pos3 = lists:nth(Position + 4, Program),
            Value1 = lists:nth(Pos1 + 1, Program),
            Value2 = lists:nth(Pos2 + 1, Program),
            Result = Value1 * Value2,
            UpdatedProgram = lists:sublist(Program, Pos3) ++ [Result] ++ lists:nthtail(Pos3 + 1, Program),
            run_program(UpdatedProgram, Position + 4)
    end.
