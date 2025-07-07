
function main()
    fid = fopen('input.txt', 'r');
    program = fscanf(fid, '%d,');
    fclose(fid);

    resultPart1 = runIntcode(program, 12, 2);
    fprintf('Part 1 Result: %d\n', resultPart1(1));

    targetOutput = 19690720;
    [noun, verb] = findNounAndVerb(program, targetOutput);
    resultPart2 = 100 * noun + verb;
    fprintf('Part 2 Result: %d\n', resultPart2);
end

function memory = runIntcode(program, noun, verb)
    memory = program;
    memory(2) = noun;
    memory(3) = verb;
    pointer = 1;

    while true
        opcode = memory(pointer);
        if opcode == 99
            break;
        end

        param1 = memory(memory(pointer + 1) + 1);
        param2 = memory(memory(pointer + 2) + 1);
        outputPos = memory(pointer + 3) + 1;

        if opcode == 1
            memory(outputPos) = param1 + param2;
        elseif opcode == 2
            memory(outputPos) = param1 * param2;
        end
        pointer = pointer + 4;
    end
end

function [noun, verb] = findNounAndVerb(program, target)
    for noun = 0:99
        for verb = 0:99
            output = runIntcode(program, noun, verb);
            if output(1) == target
                return;
            end
        end
    end
end

main();
