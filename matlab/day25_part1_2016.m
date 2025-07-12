
function main()
    registers = zeros(1, 4);
    instructions = parseInput();

    for a = 0:1000
        registers(1) = a;
        registers(2) = 0;
        registers(3) = 0;
        registers(4) = 0;
        if producesClockSignal(registers, instructions)
            fprintf('%d\n', a);
            return;
        end
    end
end

function instructions = parseInput()
    fid = fopen('input.txt', 'r');
    lines = textscan(fid, '%s', 'Delimiter', '\n');
    fclose(fid);
    lines = lines{1};

    numInstructions = length(lines);
    instructions = cell(numInstructions, 3);

    for i = 1:numInstructions
        parts = strsplit(lines{i});
        instructions{i, 1} = parts{1};
        if length(parts) > 1
            instructions{i, 2} = parts{2};
        end
        if length(parts) > 2
            instructions{i, 3} = parts{3};
        end
    end
end

function result = producesClockSignal(registers, instructions)
    lastOutput = -1;
    outputCount = 0;
    i = 1;
    numInstructions = size(instructions, 1);

    while i <= numInstructions
        instruction = instructions{i, 1};
        arg1 = instructions{i, 2};
        arg2 = instructions{i, 3};

        switch instruction
            case 'cpy'
                val1 = getValue(arg1, registers);
                registers(arg2(1) - 'a' + 1) = val1;
            case 'inc'
                registers(arg1(1) - 'a' + 1) = registers(arg1(1) - 'a' + 1) + 1;
            case 'dec'
                registers(arg1(1) - 'a' + 1) = registers(arg1(1) - 'a' + 1) - 1;
            case 'jnz'
                val1 = getValue(arg1, registers);
                if val1 ~= 0
                    val2 = getValue(arg2, registers);
                    i = i + val2;
                    continue;
                end
            case 'out'
                val = getValue(arg1, registers);
                if val ~= 0 && val ~= 1
                    result = false;
                    return;
                end
                if outputCount > 0 && val == lastOutput
                    result = false;
                    return;
                end
                lastOutput = val;
                outputCount = outputCount + 1;
                if outputCount > 50
                    result = true;
                    return;
                end
        end
        i = i + 1;
    end
    result = false;
end

function val = getValue(s, registers)
    if ~isempty(s)
        if isdigit(s(1)) || (s(1) == '-' && length(s) > 1 && isdigit(s(2)))
            val = str2double(s);
        else
            val = registers(s(1) - 'a' + 1);
        end
    else
        val = 0;
    end
end

main();
