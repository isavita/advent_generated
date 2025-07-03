
function main()
    fid = fopen('input.txt', 'r');
    instructions = textscan(fid, '%s', 'Delimiter', '\n');
    fclose(fid);
    instructions = instructions{1};

    registers = containers.Map('KeyType', 'char', 'ValueType', 'double');
    current_instruction = 1;
    last_sound = 0;

    while current_instruction >= 1 && current_instruction <= length(instructions)
        parts = strsplit(instructions{current_instruction});
        command = parts{1};
        reg = parts{2};

        if ~isKey(registers, reg)
            registers(reg) = 0;
        end

        value = 0;
        if length(parts) == 3
            val_str = parts{3};
            if isletter(val_str(1))
                if isKey(registers, val_str)
                    value = registers(val_str);
                else
                    value = 0;
                end
            else
                value = str2double(val_str);
            end
        end

        switch command
            case 'snd'
                last_sound = registers(reg);
            case 'set'
                registers(reg) = value;
            case 'add'
                registers(reg) = registers(reg) + value;
            case 'mul'
                registers(reg) = registers(reg) * value;
            case 'mod'
                registers(reg) = mod(registers(reg), value);
            case 'rcv'
                if registers(reg) ~= 0
                    disp(last_sound);
                    return;
                end
            case 'jgz'
                if registers(reg) > 0
                    current_instruction = current_instruction + value;
                    continue;
                end
        end
        current_instruction = current_instruction + 1;
    end
end

main();
