
function main()
    fileId = fopen('input.txt', 'r');
    instructions = textscan(fileId, '%s %s %d if %s %s %d', 'Delimiter', ' ');
    fclose(fileId);

    registers = containers.Map('KeyType', 'char', 'ValueType', 'double');
    maxValue = 0;

    for i = 1:length(instructions{1})
        reg = instructions{1}{i};
        op = instructions{2}{i};
        val = instructions{3}(i);
        condReg = instructions{4}{i};
        condOp = instructions{5}{i};
        condVal = instructions{6}(i);

        if ~isKey(registers, reg)
            registers(reg) = 0;
        end
        if ~isKey(registers, condReg)
            registers(condReg) = 0;
        end

        conditionMet = false;
        switch condOp
            case '=='
                conditionMet = registers(condReg) == condVal;
            case '!='
                conditionMet = registers(condReg) ~= condVal;
            case '<'
                conditionMet = registers(condReg) < condVal;
            case '<='
                conditionMet = registers(condReg) <= condVal;
            case '>'
                conditionMet = registers(condReg) > condVal;
            case '>='
                conditionMet = registers(condReg) >= condVal;
        end

        if conditionMet
            if strcmp(op, 'inc')
                registers(reg) = registers(reg) + val;
            else
                registers(reg) = registers(reg) - val;
            end
            maxValue = max(maxValue, registers(reg));
        end
    end

    if isempty(registers)
        disp(0);
    else
        disp(max(cell2mat(values(registers))));
    end
end
