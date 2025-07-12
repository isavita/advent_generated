
function main()
    registers = zeros(1, 26);
    mulCount = 0;
    
    fid = fopen('input.txt', 'r');
    instructions = textscan(fid, '%s %s %s', 'Delimiter', ' ');
    fclose(fid);
    
    instructionCount = length(instructions{1});
    pointer = 1;
    
    while pointer >= 1 && pointer <= instructionCount
        cmd = instructions{1}{pointer};
        x = instructions{2}{pointer};
        y = instructions{3}{pointer};
        
        switch cmd
            case 'set'
                registers(charToIdx(x)) = getValue(y, registers);
            case 'sub'
                registers(charToIdx(x)) = registers(charToIdx(x)) - getValue(y, registers);
            case 'mul'
                registers(charToIdx(x)) = registers(charToIdx(x)) * getValue(y, registers);
                mulCount = mulCount + 1;
            case 'jnz'
                if getValue(x, registers) ~= 0
                    pointer = pointer + getValue(y, registers) - 1;
                end
        end
        pointer = pointer + 1;
    end
    
    fprintf('%d\n', mulCount);
end

function val = getValue(s, registers)
    if isletter(s(1))
        val = registers(charToIdx(s));
    else
        val = str2double(s);
    end
end

function idx = charToIdx(c)
    idx = double(c(1)) - double('a') + 1;
end

main();
