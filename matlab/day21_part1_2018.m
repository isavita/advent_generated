
function solve()
    fid = fopen('input.txt', 'r');
    instructions = {};
    ipReg = 0;
    line = fgetl(fid);
    while ischar(line)
        if startsWith(line, '#ip')
            ipReg = sscanf(line, '#ip %d');
        else
            parts = strsplit(line);
            name = parts{1};
            abc = cellfun(@str2double, parts(2:end));
            instructions{end+1} = struct('name', name, 'abcValues', abc);
        end
        line = fgetl(fid);
    end
    fclose(fid);

    registers = zeros(1, 6);
    ip = 0;

    while true
        if ip < 0 || ip >= length(instructions)
            break;
        end

        registers(ipReg + 1) = ip;
        instruction = instructions{ip + 1};
        opcodeFunc = str2func(instruction.name);
        registers = opcodeFunc(registers, instruction.abcValues);
        ip = registers(ipReg + 1) + 1;

        if ip >= length(instructions)
            break;
        end
        if ip == 28
            break;
        end
    end

    fprintf('%d\n', registers(6));
end

function registers = addr(registers, abc)
    registers(abc(3) + 1) = registers(abc(1) + 1) + registers(abc(2) + 1);
end

function registers = addi(registers, abc)
    registers(abc(3) + 1) = registers(abc(1) + 1) + abc(2);
end

function registers = mulr(registers, abc)
    registers(abc(3) + 1) = registers(abc(1) + 1) * registers(abc(2) + 1);
end

function registers = muli(registers, abc)
    registers(abc(3) + 1) = registers(abc(1) + 1) * abc(2);
end

function registers = banr(registers, abc)
    registers(abc(3) + 1) = bitand(registers(abc(1) + 1), registers(abc(2) + 1));
end

function registers = bani(registers, abc)
    registers(abc(3) + 1) = bitand(registers(abc(1) + 1), abc(2));
end

function registers = borr(registers, abc)
    registers(abc(3) + 1) = bitor(registers(abc(1) + 1), registers(abc(2) + 1));
end

function registers = bori(registers, abc)
    registers(abc(3) + 1) = bitor(registers(abc(1) + 1), abc(2));
end

function registers = setr(registers, abc)
    registers(abc(3) + 1) = registers(abc(1) + 1);
end

function registers = seti(registers, abc)
    registers(abc(3) + 1) = abc(1);
end

function registers = gtir(registers, abc)
    registers(abc(3) + 1) = abc(1) > registers(abc(2) + 1);
end

function registers = gtri(registers, abc)
    registers(abc(3) + 1) = registers(abc(1) + 1) > abc(2);
end

function registers = gtrr(registers, abc)
    registers(abc(3) + 1) = registers(abc(1) + 1) > registers(abc(2) + 1);
end

function registers = eqir(registers, abc)
    registers(abc(3) + 1) = abc(1) == registers(abc(2) + 1);
end

function registers = eqri(registers, abc)
    registers(abc(3) + 1) = registers(abc(1) + 1) == abc(2);
end

function registers = eqrr(registers, abc)
    registers(abc(3) + 1) = registers(abc(1) + 1) == registers(abc(2) + 1);
end

main();
