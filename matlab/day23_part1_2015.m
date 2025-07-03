
function main()
    fid = fopen('input.txt', 'r');
    instructions = textscan(fid, '%s', 'Delimiter', '\n');
    fclose(fid);
    instructions = instructions{1};

    registers.a = 0;
    registers.b = 0;
    current_instruction = 1;

    while current_instruction <= length(instructions)
        parts = strsplit(instructions{current_instruction});
        opcode = parts{1};
        operand1 = parts{2};

        switch opcode
            case 'hlf'
                registers.(operand1) = floor(registers.(operand1) / 2);
                current_instruction = current_instruction + 1;
            case 'tpl'
                registers.(operand1) = registers.(operand1) * 3;
                current_instruction = current_instruction + 1;
            case 'inc'
                registers.(operand1) = registers.(operand1) + 1;
                current_instruction = current_instruction + 1;
            case 'jmp'
                offset = str2double(operand1);
                current_instruction = current_instruction + offset;
            case 'jie'
                register_name = operand1(1);
                offset = str2double(parts{3});
                if mod(registers.(register_name), 2) == 0
                    current_instruction = current_instruction + offset;
                else
                    current_instruction = current_instruction + 1;
                end
            case 'jio'
                register_name = operand1(1);
                offset = str2double(parts{3});
                if registers.(register_name) == 1
                    current_instruction = current_instruction + offset;
                else
                    current_instruction = current_instruction + 1;
                end
        end
    end

    fprintf('%d\n', registers.b);
end

main();
