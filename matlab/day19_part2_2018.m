
function main()
    ops = containers.Map;
    ops('addr') = @(r, a, b) r(a+1) + r(b+1);
    ops('addi') = @(r, a, b) r(a+1) + b;
    ops('mulr') = @(r, a, b) r(a+1) * r(b+1);
    ops('muli') = @(r, a, b) r(a+1) * b;
    ops('banr') = @(r, a, b) bitand(r(a+1), r(b+1));
    ops('bani') = @(r, a, b) bitand(r(a+1), b);
    ops('borr') = @(r, a, b) bitor(r(a+1), r(b+1));
    ops('bori') = @(r, a, b) bitor(r(a+1), b);
    ops('setr') = @(r, a, b) r(a+1);
    ops('seti') = @(r, a, b) a;
    ops('gtir') = @(r, a, b) double(a > r(b+1));
    ops('gtri') = @(r, a, b) double(r(a+1) > b);
    ops('gtrr') = @(r, a, b) double(r(a+1) > r(b+1));
    ops('eqir') = @(r, a, b) double(a == r(b+1));
    ops('eqri') = @(r, a, b) double(r(a+1) == b);
    ops('eqrr') = @(r, a, b) double(r(a+1) == r(b+1));

    fid = fopen('input.txt', 'r');
    if fid == -1
        error('Failed to open input.txt');
    end

    program = {};
    ipRegister = 0;

    while ~feof(fid)
        line = fgetl(fid);
        if isempty(line) || line(1) == '%'
            continue;
        end

        if line(1) == '#'
            ipRegister = sscanf(line, '#ip %d');
            continue;
        end

        data = textscan(line, '%s %d %d %d');
        opcode = data{1}{1};
        a = data{2};
        b = data{3};
        c = data{4};

        inst.op = ops(opcode);
        inst.a = a;
        inst.b = b;
        inst.c = c;
        program{end+1} = inst;
    end
    fclose(fid);

    registers = zeros(1, 6);
    registers(1) = 1;

    registers = runProgram(ipRegister, program, registers, 1000);

    n = max(registers);
    total = 0;
    
    limit = floor(sqrt(n));
    for i = 1:limit
        if mod(n, i) == 0
            total = total + i;
            if i ~= n/i 
                total = total + n/i;
            end
        end
    end

    fprintf('%d\n', total);
end

function registers = runProgram(ipRegister, program, registers, maxCycles)
    ip = 0;
    cycles = 0;
    programLength = length(program);

    while ip >= 0 && ip < programLength
        registers(ipRegister + 1) = ip; 
        
        current_instruction = program{ip + 1}; 
        
        registers(current_instruction.c + 1) = current_instruction.op(registers, current_instruction.a, current_instruction.b);
        
        ip = registers(ipRegister + 1) + 1; 
        
        cycles = cycles + 1;
        if maxCycles > 0 && cycles >= maxCycles
            break;
        end
    end
end
