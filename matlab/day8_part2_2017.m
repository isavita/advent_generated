
function solve_register_instructions()
    % SOLVE_REGISTER_INSTRUCTIONS Reads instructions from input.txt,
    % processes them, and prints the largest register value and the
    % highest value ever held by any register to standard output.

    % Initialize registers and tracking variables
    registers = containers.Map('KeyType', 'char', 'ValueType', 'int64');
    max_ever_value = int64(0);

    % Read instructions from input.txt
    try
        fid = fopen('input.txt', 'r');
        if fid == -1
            error('Could not open input.txt for reading.');
        end
        instructions = textscan(fid, '%s', 'Delimiter', '\n');
        fclose(fid);
        instructions = instructions{1}; % Extract the cell array of strings
    catch ME
        fprintf(2, 'Error reading input file: %s\n', ME.message);
        return;
    end

    % Process each instruction
    for i = 1:length(instructions)
        instruction_str = instructions{i};

        % Parse the instruction string
        % Example: "b inc 5 if a > 1"
        % Parts: register_to_modify, operation, value, 'if', condition_register,
        %        condition_operator, condition_value
        parts = strsplit(instruction_str);

        reg_to_modify = parts{1};
        operation = parts{2};
        value = str2double(parts{3});
        condition_reg = parts{5};
        condition_op = parts{6};
        condition_val = str2double(parts{7});

        % Ensure registers exist, initializing to 0 if new
        if ~isKey(registers, reg_to_modify)
            registers(reg_to_modify) = int64(0);
        end
        if ~isKey(registers, condition_reg)
            registers(condition_reg) = int64(0);
        end

        % Evaluate the condition
        condition_met = false;
        current_condition_reg_val = registers(condition_reg);

        switch condition_op
            case '=='
                condition_met = (current_condition_reg_val == condition_val);
            case '!='
                condition_met = (current_condition_reg_val ~= condition_val);
            case '<'
                condition_met = (current_condition_reg_val < condition_val);
            case '<='
                condition_met = (current_condition_reg_val <= condition_val);
            case '>'
                condition_met = (current_condition_reg_val > condition_val);
            case '>='
                condition_met = (current_condition_reg_val >= condition_val);
            otherwise
                fprintf(2, 'Warning: Unknown condition operator "%s" in instruction: %s\n', condition_op, instruction_str);
        end

        % If the condition is met, perform the modification
        if condition_met
            current_reg_val = registers(reg_to_modify);
            if strcmp(operation, 'inc')
                new_reg_val = current_reg_val + value;
            elseif strcmp(operation, 'dec')
                new_reg_val = current_reg_val - value;
            else
                fprintf(2, 'Warning: Unknown operation "%s" in instruction: %s\n', operation, instruction_str);
                continue; % Skip to next instruction if operation is unknown
            end
            registers(reg_to_modify) = new_reg_val;

            % Update the highest value ever held
            max_ever_value = max(max_ever_value, new_reg_val);
        end
    end

    % Find the largest value in any register after all instructions
    if isempty(registers)
        max_final_value = int64(0);
    else
        max_final_value = max(cell2mat(values(registers)));
    end

    % Print the results to standard output
    fprintf('Largest value in any register: %d\n', max_final_value);
    fprintf('Highest value ever held by any register: %d\n', max_ever_value);
end

% --- Main Entry Point ---
% This function is designed to be called directly.
% To run it, save this code as 'solve_register_instructions.m'
% and then execute 'solve_register_instructions' in the MATLAB command window.
% Ensure 'input.txt' is in the same directory or provide the full path.

% Example of how to call the function:
% solve_register_instructions();
