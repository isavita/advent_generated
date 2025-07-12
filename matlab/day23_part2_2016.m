
function solve()
    registers = [12, 0, 0, 0];

    fid = fopen('input.txt', 'r');
    if fid == -1
        error('Error opening input.txt');
    end

    instructions(1000) = struct('op', '', 'arg1_type', 0, 'arg1_val', 0, 'arg2_type', 0, 'arg2_val', 0);
    instr_count = 0;

    while ~feof(fid)
        line = fgetl(fid);
        if ~ischar(line), break, end

        tokens = strsplit(line, ' ');
        
        instr_count = instr_count + 1;
        current_instr.op = tokens{1};
        current_instr.arg1_type = 0;
        current_instr.arg1_val = 0;
        current_instr.arg2_type = 0;
        current_instr.arg2_val = 0;

        if numel(tokens) >= 2
            arg1_str = tokens{2};
            if is_reg(arg1_str)
                current_instr.arg1_type = 0;
                current_instr.arg1_val = reg_to_idx(arg1_str);
            else
                current_instr.arg1_type = 1;
                current_instr.arg1_val = str2double(arg1_str);
            end
        end

        if numel(tokens) >= 3
            arg2_str = tokens{3};
            if is_reg(arg2_str)
                current_instr.arg2_type = 0;
                current_instr.arg2_val = reg_to_idx(arg2_str);
            else
                current_instr.arg2_type = 1;
                current_instr.arg2_val = str2double(arg2_str);
            end
        end
        
        instructions(instr_count) = current_instr;
    end
    fclose(fid);

    instructions = instructions(1:instr_count);
    num_instructions = numel(instructions);
    
    i = 1;

    while i <= num_instructions
        if i + 5 <= num_instructions
            instr1 = instructions(i);
            instr2 = instructions(i+1);
            instr3 = instructions(i+2);
            instr4 = instructions(i+3);
            instr5 = instructions(i+4);
            instr6 = instructions(i+5);

            is_opt_pattern = false;
            if strcmp(instr1.op, 'cpy') && instr1.arg1_type == 0 && instr1.arg2_type == 0 && ...
               strcmp(instr2.op, 'inc') && instr2.arg1_type == 0 && ...
               strcmp(instr3.op, 'dec') && instr3.arg1_type == 0 && ...
               strcmp(instr4.op, 'jnz') && instr4.arg1_type == 0 && instr4.arg2_type == 1 && instr4.arg2_val == -2 && ...
               strcmp(instr5.op, 'dec') && instr5.arg1_type == 0 && ...
               strcmp(instr6.op, 'jnz') && instr6.arg1_type == 0 && instr6.arg2_type == 1 && instr6.arg2_val == -5

                Y_reg_idx = instr1.arg1_val;
                Z_reg_idx_cpy = instr1.arg2_val;
                X_reg_idx = instr2.arg1_val;
                Z_reg_idx_dec = instr3.arg1_val;
                Z_reg_idx_jnz = instr4.arg1_val;
                W_reg_idx_dec = instr5.arg1_val;
                W_reg_idx_jnz = instr6.arg1_val;

                if Z_reg_idx_cpy == Z_reg_idx_dec && Z_reg_idx_dec == Z_reg_idx_jnz && ...
                   W_reg_idx_dec == W_reg_idx_jnz && ...
                   X_reg_idx ~= Z_reg_idx_cpy && X_reg_idx ~= W_reg_idx_dec && ...
                   Y_reg_idx ~= Z_reg_idx_cpy && Y_reg_idx ~= W_reg_idx_dec
                    is_opt_pattern = true;
                end
            end

            if is_opt_pattern
                val_Y = registers(Y_reg_idx);
                val_W = registers(W_reg_idx_dec);

                registers(X_reg_idx) = registers(X_reg_idx) + val_Y * val_W;
                registers(Z_reg_idx_cpy) = 0;
                registers(W_reg_idx_dec) = 0;

                i = i + 6;
                continue;
            end
        end

        instr = instructions(i);

        if instr.arg1_type == 0
            val1 = registers(instr.arg1_val);
        else
            val1 = instr.arg1_val;
        end

        switch instr.op
            case 'cpy'
                if instr.arg2_type == 0
                    registers(instr.arg2_val) = val1;
                end
            case 'inc'
                if instr.arg1_type == 0
                    registers(instr.arg1_val) = registers(instr.arg1_val) + 1;
                end
            case 'dec'
                if instr.arg1_type == 0
                    registers(instr.arg1_val) = registers(instr.arg1_val) - 1;
                end
            case 'jnz'
                if val1 ~= 0
                    if instr.arg2_type == 0
                        val2 = registers(instr.arg2_val);
                    else
                        val2 = instr.arg2_val;
                    end
                    i = i + val2;
                    continue;
                end
            case 'tgl'
                target_index = i + val1;

                if target_index >= 1 && target_index <= num_instructions
                    target_instr = instructions(target_index);
                    
                    switch target_instr.op
                        case 'inc'
                            instructions(target_index).op = 'dec';
                        case {'dec', 'tgl'}
                            instructions(target_index).op = 'inc';
                        case 'jnz'
                            instructions(target_index).op = 'cpy';
                        case 'cpy'
                            instructions(target_index).op = 'jnz';
                    end
                end
        end
        i = i + 1;
    end

    fprintf('%d\n', registers(1));
end

function res = is_reg(s)
    res = (numel(s) == 1 && s(1) >= 'a' && s(1) <= 'd');
end

function idx = reg_to_idx(c)
    idx = (c - 'a' + 1);
end
