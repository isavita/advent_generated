-module(task).
-export([call/0]).

read_input(FileName) ->
    {ok, File} = file:read_file(FileName),
    Instructions = string:tokens(binary_to_list(File), "\n"),
    Instructions.

execute_instruction({Reg, Op, Amount, CondReg, CondOp, CondVal}, Registers, Max) ->
    case CondOp of
        ">"  -> C = maps:get(CondReg, Registers, 0) > CondVal;
        "<"  -> C = maps:get(CondReg, Registers, 0) < CondVal;
        ">=" -> C = maps:get(CondReg, Registers, 0) >= CondVal;
        "<=" -> C = maps:get(CondReg, Registers, 0) =< CondVal;
        "==" -> C = maps:get(CondReg, Registers, 0) == CondVal;
        "!=" -> C = maps:get(CondReg, Registers, 0) /= CondVal
    end,
    case C of
        true  -> 
            NewVal = case Op of
                "inc" -> maps:get(Reg, Registers, 0) + Amount;
                "dec" -> maps:get(Reg, Registers, 0) - Amount
            end,
            NewRegisters = maps:put(Reg, NewVal, Registers),
            NewMax = max(NewVal, Max),
            {NewRegisters, NewMax};
        false -> {Registers, Max}
    end.

execute_instructions([], Registers, Max) -> {Registers, Max};
execute_instructions([I|Is], Registers, Max) ->
    [Reg, Op, Amt, _, CondReg, CondOp, CondVal] = string:tokens(I, " "),
    Amount = list_to_integer(Amt),
    CondValue = list_to_integer(CondVal),
    Instruction = {Reg, Op, Amount, CondReg, CondOp, CondValue},
    {NewRegisters, NewMax} = execute_instruction(Instruction, Registers, Max),
    execute_instructions(Is, NewRegisters, NewMax).

call() ->
    Instructions = read_input("input.txt"),
    {FinalRegisters, _MaxValue} = execute_instructions(Instructions, #{}, 0),
    Answer1 = lists:max(maps:values(FinalRegisters)),
    io:format("Part 1: ~p~n", [Answer1]).
