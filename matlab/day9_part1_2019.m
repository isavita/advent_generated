
function main()
    fid = fopen('input.txt', 'r');
    programStr = fscanf(fid, '%s');
    fclose(fid);

    program = str2num(['[', programStr, ']']);
    memory = containers.Map('KeyType', 'double', 'ValueType', 'double');
    for i = 0:length(program)-1
        memory(i) = program(i+1);
    end

    fprintf('%d\n', runIntcode(memory));
end

function output = runIntcode(memory)
    ip = 0;
    relativeBase = 0;
    output = 0;

    while true
        instruction = memory(ip);
        opcode = mod(instruction, 100);
        modes = floor(instruction / 100);

        getParam = @(offset) getParameter(memory, ip, offset, relativeBase, modes);
        setParam = @(offset, value) setParameter(memory, ip, offset, relativeBase, modes, value);

        switch opcode
            case 1
                setParam(3, getParam(1) + getParam(2));
                ip = ip + 4;
            case 2
                setParam(3, getParam(1) * getParam(2));
                ip = ip + 4;
            case 3
                setParam(1, 1);
                ip = ip + 2;
            case 4
                output = getParam(1);
                ip = ip + 2;
            case 5
                if getParam(1) ~= 0
                    ip = getParam(2);
                else
                    ip = ip + 3;
                end
            case 6
                if getParam(1) == 0
                    ip = getParam(2);
                else
                    ip = ip + 3;
                end
            case 7
                if getParam(1) < getParam(2)
                    setParam(3, 1);
                else
                    setParam(3, 0);
                end
                ip = ip + 4;
            case 8
                if getParam(1) == getParam(2)
                    setParam(3, 1);
                else
                    setParam(3, 0);
                end
                ip = ip + 4;
            case 9
                relativeBase = relativeBase + getParam(1);
                ip = ip + 2;
            case 99
                return;
            otherwise
                error('unknown opcode: %d', opcode);
        end
    end
end

function value = getParameter(memory, ip, offset, relativeBase, modes)
    mode = floor(mod(modes, 10^(offset)) / 10^(offset-1));
    param = memory(ip + offset);
    switch mode
        case 0
            if memory.isKey(param)
                value = memory(param);
            else
                value = 0;
            end
        case 1
            value = param;
        case 2
            if memory.isKey(relativeBase + param)
                value = memory(relativeBase + param);
            else
                value = 0;
            end
        otherwise
            error('unknown parameter mode');
    end
end

function setParameter(memory, ip, offset, relativeBase, modes, value)
    mode = floor(mod(modes, 10^(offset)) / 10^(offset-1));
    param = memory(ip + offset);
    switch mode
        case 0
            memory(param) = value;
        case 2
            memory(relativeBase + param) = value;
        otherwise
            error('unknown parameter mode for set');
    end
end

main();
