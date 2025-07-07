
function main()
    program = readInput('input.txt');
    output = runIntcode(program, 2);
    disp(output);
end

function program = readInput(filePath)
    fid = fopen(filePath, 'r');
    programStr = fscanf(fid, '%s');
    fclose(fid);
    program = cellfun(@str2double, strsplit(programStr, ','));
end

function output = runIntcode(program, inputVal)
    memory = program;
    pointer = 1;
    relativeBase = 0;
    
    while true
        instruction = memory(pointer);
        opcode = mod(instruction, 100);
        modes = [mod(floor(instruction / 100), 10), mod(floor(instruction / 1000), 10), mod(floor(instruction / 10000), 10)];
        
        switch opcode
            case 1
                [memory, pointer] = opAdd(memory, pointer, modes, relativeBase);
            case 2
                [memory, pointer] = opMultiply(memory, pointer, modes, relativeBase);
            case 3
                [memory, pointer] = opInput(memory, pointer, modes, relativeBase, inputVal);
            case 4
                output = opOutput(memory, pointer, modes, relativeBase);
                return;
            case 5
                pointer = opJumpIfTrue(memory, pointer, modes, relativeBase);
            case 6
                pointer = opJumpIfFalse(memory, pointer, modes, relativeBase);
            case 7
                [memory, pointer] = opLessThan(memory, pointer, modes, relativeBase);
            case 8
                [memory, pointer] = opEquals(memory, pointer, modes, relativeBase);
            case 9
                relativeBase = opAdjustRelativeBase(memory, pointer, modes, relativeBase);
                pointer = pointer + 2;
            case 99
                return;
            otherwise
                error('Unknown opcode: %d', opcode);
        end
    end
end

function [memory, pointer] = opAdd(memory, pointer, modes, relativeBase)
    val1 = getParam(memory, pointer, 1, modes(1), relativeBase);
    val2 = getParam(memory, pointer, 2, modes(2), relativeBase);
    memory = setParam(memory, pointer, 3, modes(3), relativeBase, val1 + val2);
    pointer = pointer + 4;
end

function [memory, pointer] = opMultiply(memory, pointer, modes, relativeBase)
    val1 = getParam(memory, pointer, 1, modes(1), relativeBase);
    val2 = getParam(memory, pointer, 2, modes(2), relativeBase);
    memory = setParam(memory, pointer, 3, modes(3), relativeBase, val1 * val2);
    pointer = pointer + 4;
end

function [memory, pointer] = opInput(memory, pointer, modes, relativeBase, inputVal)
    memory = setParam(memory, pointer, 1, modes(1), relativeBase, inputVal);
    pointer = pointer + 2;
end

function output = opOutput(memory, pointer, modes, relativeBase)
    output = getParam(memory, pointer, 1, modes(1), relativeBase);
end

function pointer = opJumpIfTrue(memory, pointer, modes, relativeBase)
    val1 = getParam(memory, pointer, 1, modes(1), relativeBase);
    val2 = getParam(memory, pointer, 2, modes(2), relativeBase);
    if val1 ~= 0
        pointer = val2 + 1;
    else
        pointer = pointer + 3;
    end
end

function pointer = opJumpIfFalse(memory, pointer, modes, relativeBase)
    val1 = getParam(memory, pointer, 1, modes(1), relativeBase);
    val2 = getParam(memory, pointer, 2, modes(2), relativeBase);
    if val1 == 0
        pointer = val2 + 1;
    else
        pointer = pointer + 3;
    end
end

function [memory, pointer] = opLessThan(memory, pointer, modes, relativeBase)
    val1 = getParam(memory, pointer, 1, modes(1), relativeBase);
    val2 = getParam(memory, pointer, 2, modes(2), relativeBase);
    memory = setParam(memory, pointer, 3, modes(3), relativeBase, double(val1 < val2));
    pointer = pointer + 4;
end

function [memory, pointer] = opEquals(memory, pointer, modes, relativeBase)
    val1 = getParam(memory, pointer, 1, modes(1), relativeBase);
    val2 = getParam(memory, pointer, 2, modes(2), relativeBase);
    memory = setParam(memory, pointer, 3, modes(3), relativeBase, double(val1 == val2));
    pointer = pointer + 4;
end

function relativeBase = opAdjustRelativeBase(memory, pointer, modes, relativeBase)
    val1 = getParam(memory, pointer, 1, modes(1), relativeBase);
    relativeBase = relativeBase + val1;
end

function val = getParam(memory, pointer, offset, mode, relativeBase)
    paramIndex = pointer + offset;
    if paramIndex > length(memory)
        val = 0;
    else
        param = memory(paramIndex);
        switch mode
            case 0
                if param + 1 > length(memory)
                    val = 0;
                else
                    val = memory(param + 1);
                end
            case 1
                val = param;
            case 2
                if relativeBase + param + 1 > length(memory)
                    val = 0;
                else
                    val = memory(relativeBase + param + 1);
                end
            otherwise
                error('Unknown mode: %d', mode);
        end
    end
end

function memory = setParam(memory, pointer, offset, mode, relativeBase, value)
    paramIndex = pointer + offset;
    if paramIndex > length(memory)
        memory(end+1:paramIndex) = 0;
    end
    param = memory(paramIndex);
    
    writeIndex = 0;
    switch mode
        case 0
            writeIndex = param + 1;
        case 2
            writeIndex = relativeBase + param + 1;
        otherwise
            error('Unknown mode for writing: %d', mode);
    end
    
    if writeIndex > length(memory)
        memory(end+1:writeIndex) = 0;
    end
    memory(writeIndex) = value;
end

main();
