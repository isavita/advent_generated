
function main()
    registers = struct('a', 0, 'b', 0, 'c', 0, 'd', 0);
    
    fid = fopen('input.txt', 'r');
    instructions = textscan(fid, '%s', 'Delimiter', '\n');
    fclose(fid);
    instructions = instructions{1};
    
    numInstructions = length(instructions);
    i = 1;
    
    while i <= numInstructions
        parts = strsplit(instructions{i});
        
        switch parts{1}
            case 'cpy'
                val = getValue(registers, parts{2});
                registers.(parts{3}) = val;
            case 'inc'
                registers.(parts{2}) = registers.(parts{2}) + 1;
            case 'dec'
                registers.(parts{2}) = registers.(parts{2}) - 1;
            case 'jnz'
                val = getValue(registers, parts{2});
                if val ~= 0
                    jump = str2double(parts{3});
                    i = i + jump;
                    continue;
                end
        end
        i = i + 1;
    end
    
    fprintf('%d\n', registers.a);
end

function val = getValue(registers, x)
    if isletter(x)
        val = registers.(x);
    else
        val = str2double(x);
    end
end

main();
