
function main()
    instructions = readInstructions('input.txt');
    registers = struct('a', 7, 'b', 0, 'c', 0, 'd', 0);
    registers = executeInstructions(instructions, registers);
    disp(registers.a);
end

function instructions = readInstructions(filename)
    fid = fopen(filename, 'r');
    if fid == -1
        error('Cannot open file: %s', filename);
    end
    instructions = textscan(fid, '%s', 'Delimiter', '\n');
    fclose(fid);
    instructions = instructions{1};
end

function registers = executeInstructions(instructions, registers)
    pc = 1;
    numInstructions = length(instructions);
    while pc <= numInstructions
        parts = strsplit(instructions{pc});
        op = parts{1};
        
        switch op
            case 'cpy'
                val = getValue(parts{2}, registers);
                if isfield(registers, parts{3})
                    registers.(parts{3}) = val;
                end
            case 'inc'
                if isfield(registers, parts{2})
                    registers.(parts{2}) = registers.(parts{2}) + 1;
                end
            case 'dec'
                if isfield(registers, parts{2})
                    registers.(parts{2}) = registers.(parts{2}) - 1;
                end
            case 'jnz'
                val1 = getValue(parts{2}, registers);
                if val1 ~= 0
                    val2 = getValue(parts{3}, registers);
                    pc = pc + val2 - 1;
                end
            case 'tgl'
                val = getValue(parts{2}, registers);
                targetPc = pc + val;
                if targetPc >= 1 && targetPc <= numInstructions
                    instructions{targetPc} = toggleInstruction(instructions{targetPc});
                end
        end
        pc = pc + 1;
    end
end

function val = getValue(s, registers)
    if ~isnan(str2double(s))
        val = str2double(s);
    else
        val = registers.(s);
    end
end

function toggledInstr = toggleInstruction(instr)
    parts = strsplit(instr);
    op = parts{1};
    switch op
        case 'inc'
            parts{1} = 'dec';
        case {'dec', 'tgl'}
            parts{1} = 'inc';
        case 'jnz'
            parts{1} = 'cpy';
        case 'cpy'
            parts{1} = 'jnz';
    end
    toggledInstr = strjoin(parts, ' ');
end

main();
