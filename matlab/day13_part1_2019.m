
function main()
    program = readProgram("input.txt");
    result = countBlocks(program);
    disp(result);
end

function program = readProgram(filepath)
    fid = fopen(filepath, 'r');
    programStr = fscanf(fid, '%s');
    fclose(fid);
    program = cellfun(@str2double, strsplit(programStr, ','));
end

function blockCount = countBlocks(program)
    output = runIntcode(program);
    grid = containers.Map('KeyType', 'char', 'ValueType', 'double');
    for i = 1:3:length(output)
        x = output(i);
        y = output(i+1);
        tile = output(i+2);
        grid([num2str(x), ',', num2str(y)]) = tile;
    end
    blockCount = 0;
    keys = grid.keys;
    for i = 1:length(keys)
        if grid(keys{i}) == 2
            blockCount = blockCount + 1;
        end
    end
end

function output = runIntcode(program)
    data = containers.Map('KeyType', 'double', 'ValueType', 'double');
    for i = 1:length(program)
        data(i-1) = program(i);
    end
    ip = 0;
    relBase = 0;
    output = [];
    inputQueue = [];

    while true
        opcode = mod(data(ip), 100);
        modes = [mod(floor(data(ip)/100), 10), mod(floor(data(ip)/1000), 10), mod(floor(data(ip)/10000), 10)];

        if opcode == 99
            break;
        end

        getVal = @(idx, mode) getValue(data, ip, relBase, idx, mode);
        setVal = @(idx, mode, val) setValue(data, ip, relBase, idx, mode, val);

        if opcode == 1
            val = getVal(ip + 1, modes(1)) + getVal(ip + 2, modes(2));
            setVal(ip + 3, modes(3), val);
            ip = ip + 4;
        elseif opcode == 2
            val = getVal(ip + 1, modes(1)) * getVal(ip + 2, modes(2));
            setVal(ip + 3, modes(3), val);
            ip = ip + 4;
        elseif opcode == 3
            if isempty(inputQueue)
                error('No input available');
            end
            setVal(ip + 1, modes(1), inputQueue(1));
            inputQueue(1) = [];
            ip = ip + 2;
        elseif opcode == 4
            output(end+1) = getVal(ip + 1, modes(1));
            ip = ip + 2;
        elseif opcode == 5
            if getVal(ip + 1, modes(1)) ~= 0
                ip = getVal(ip + 2, modes(2));
            else
                ip = ip + 3;
            end
        elseif opcode == 6
            if getVal(ip + 1, modes(1)) == 0
                ip = getVal(ip + 2, modes(2));
            else
                ip = ip + 3;
            end
        elseif opcode == 7
            if getVal(ip + 1, modes(1)) < getVal(ip + 2, modes(2))
                setVal(ip + 3, modes(3), 1);
            else
                setVal(ip + 3, modes(3), 0);
            end
            ip = ip + 4;
        elseif opcode == 8
            if getVal(ip + 1, modes(1)) == getVal(ip + 2, modes(2))
                setVal(ip + 3, modes(3), 1);
            else
                setVal(ip + 3, modes(3), 0);
            end
            ip = ip + 4;
        elseif opcode == 9
            relBase = relBase + getVal(ip + 1, modes(1));
            ip = ip + 2;
        else
            error('Unknown opcode: %d', opcode);
        end
    end
end

function val = getValue(data, ip, relBase, idx, mode)
    if mode == 1
        val = data(idx);
    elseif mode == 0
        if ~isKey(data, data(idx))
            data(data(idx)) = 0;
        end
        val = data(data(idx));
    elseif mode == 2
        if ~isKey(data, relBase + data(idx))
            data(relBase + data(idx)) = 0;
        end
        val = data(relBase + data(idx));
    else
        error('Unknown mode: %d', mode);
    end
end

function setValue(data, ip, relBase, idx, mode, val)
    if mode == 0
        data(data(idx)) = val;
    elseif mode == 2
        data(relBase + data(idx)) = val;
    else
        error('Unknown mode: %d', mode);
    end
end

main();
