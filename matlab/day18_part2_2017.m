
function duet_solver()
    % Constants
    MAX_INSTRUCTIONS = 100;
    QUEUE_SIZE = 100000;
    NUM_REGISTERS = 26;

    % Instruction struct definition
    instruction_template = struct('op', '', ...
                                  'arg1_is_reg', false, 'arg1_val', int64(0), ...
                                  'arg2_is_reg', false, 'arg2_val', int64(0));
    instructions = repmat(instruction_template, MAX_INSTRUCTIONS, 1);
    instruction_count = 0;

    % Queue struct definition
    queue_template = struct('data', zeros(QUEUE_SIZE, 1, 'int64'), ...
                            'head', 1, 'tail', 1, 'count', 0);

    % Initialize registers, program counters, flags
    registers = zeros(2, NUM_REGISTERS, 'int64');
    pc = [1, 1]; % MATLAB is 1-indexed
    waiting = [false, false];
    terminated = [false, false];
    send_count1 = int64(0);

    % Initialize queues
    queue0 = queue_template;
    queue1 = queue_template;

    % --- File Parsing ---
    fid = fopen('input.txt', 'r');
    if fid == -1
        error('Error opening input.txt');
    end

    line_idx = 1;
    while ~feof(fid) && line_idx <= MAX_INSTRUCTIONS
        line = fgetl(fid);
        if ~ischar(line), break; end

        parts = strsplit(line, ' ');
        if isempty(parts), continue; end

        cmd_str = parts{1};
        
        current_instr = instruction_template;

        switch cmd_str
            case 'snd', current_instr.op = 'SND';
            case 'set', current_instr.op = 'SET';
            case 'add', current_instr.op = 'ADD';
            case 'mul', current_instr.op = 'MUL';
            case 'mod', current_instr.op = 'MOD';
            case 'rcv', current_instr.op = 'RCV';
            case 'jgz', current_instr.op = 'JGZ';
            otherwise, continue;
        end

        if length(parts) >= 2
            [current_instr.arg1_is_reg, current_instr.arg1_val] = parse_argument_matlab(parts{2});
        end

        if length(parts) >= 3
            [current_instr.arg2_is_reg, current_instr.arg2_val] = parse_argument_matlab(parts{3});
        end
        
        instruction_count = instruction_count + 1;
        instructions(instruction_count) = current_instr;
        line_idx = line_idx + 1;
    end
    fclose(fid);
    
    instructions = instructions(1:instruction_count);

    % Program 1 starts with p=1
    registers(2, 'p' - 'a' + 1) = 1;

    % --- Simulation Loop ---
    while ~((terminated(1) && terminated(2)) || (waiting(1) && waiting(2)))
        for prog_id_idx = 1:2
            prog_id = prog_id_idx - 1; % 0 for prog 0, 1 for prog 1

            if terminated(prog_id_idx) || waiting(prog_id_idx)
                continue;
            end

            should_break_inner_loop = false;
            while pc(prog_id_idx) >= 1 && pc(prog_id_idx) <= instruction_count
                instr = instructions(pc(prog_id_idx));
                
                target_reg_idx = -1;
                if instr.arg1_is_reg
                    target_reg_idx = instr.arg1_val;
                end

                switch instr.op
                    case 'SND'
                        val_to_send = get_value_matlab(instr.arg1_is_reg, instr.arg1_val, prog_id, registers);
                        if prog_id == 0
                            queue1 = queue_push_matlab(queue1, val_to_send, QUEUE_SIZE);
                        else
                            queue0 = queue_push_matlab(queue0, val_to_send, QUEUE_SIZE);
                            send_count1 = send_count1 + 1;
                        end
                    case 'SET'
                        if target_reg_idx ~= -1
                            registers(prog_id_idx, target_reg_idx + 1) = get_value_matlab(instr.arg2_is_reg, instr.arg2_val, prog_id, registers);
                        end
                    case 'ADD'
                        if target_reg_idx ~= -1
                            registers(prog_id_idx, target_reg_idx + 1) = registers(prog_id_idx, target_reg_idx + 1) + get_value_matlab(instr.arg2_is_reg, instr.arg2_val, prog_id, registers);
                        end
                    case 'MUL'
                        if target_reg_idx ~= -1
                            registers(prog_id_idx, target_reg_idx + 1) = registers(prog_id_idx, target_reg_idx + 1) * get_value_matlab(instr.arg2_is_reg, instr.arg2_val, prog_id, registers);
                        end
                    case 'MOD'
                        if target_reg_idx ~= -1
                            val2 = get_value_matlab(instr.arg2_is_reg, instr.arg2_val, prog_id, registers);
                            if val2 ~= 0
                                registers(prog_id_idx, target_reg_idx + 1) = mod(registers(prog_id_idx, target_reg_idx + 1), val2);
                            else
                                registers(prog_id_idx, target_reg_idx + 1) = 0;
                            end
                        end
                    case 'RCV'
                        if prog_id == 0
                            recv_queue = queue0;
                        else
                            recv_queue = queue1;
                        end

                        if queue_is_empty_matlab(recv_queue)
                            waiting(prog_id_idx) = true;
                            should_break_inner_loop = true;
                            break;
                        else
                            [recv_queue, received_val] = queue_pop_matlab(recv_queue, QUEUE_SIZE);
                            if target_reg_idx ~= -1
                                registers(prog_id_idx, target_reg_idx + 1) = received_val;
                            end
                            waiting(prog_id_idx) = false;
                        end
                        
                        if prog_id == 0
                            queue0 = recv_queue;
                        else
                            queue1 = recv_queue;
                        end

                    case 'JGZ'
                        if get_value_matlab(instr.arg1_is_reg, instr.arg1_val, prog_id, registers) > 0
                            pc(prog_id_idx) = pc(prog_id_idx) + get_value_matlab(instr.arg2_is_reg, instr.arg2_val, prog_id, registers);
                            continue;
                        end
                end
                pc(prog_id_idx) = pc(prog_id_idx) + 1;
                
                if strcmp(instr.op, 'SND')
                    other_prog_idx = 3 - prog_id_idx;
                    if waiting(other_prog_idx)
                        waiting(other_prog_idx) = false;
                    end
                end
            end

            if ~(pc(prog_id_idx) >= 1 && pc(prog_id_idx) <= instruction_count)
                terminated(prog_id_idx) = true;
            end
        end
    end

    fprintf('%d\n', send_count1);

end

% --- Helper Functions ---

function [is_reg, val] = parse_argument_matlab(s)
    if isempty(s)
        is_reg = false;
        val = int64(0);
        return;
    end
    
    if length(s) == 1 && isletter(s)
        is_reg = true;
        val = int64(s - 'a');
    else
        is_reg = false;
        val = int64(str2double(s));
    end
end

function val = get_value_matlab(arg_is_reg, arg_val, prog_id, registers)
    if arg_is_reg
        val = registers(prog_id + 1, arg_val + 1);
    else
        val = arg_val;
    end
end

% --- Queue Operations ---

function isEmpty = queue_is_empty_matlab(q)
    isEmpty = (q.count == 0);
end

function isFull = queue_is_full_matlab(q, queue_size)
    isFull = (q.count == queue_size);
end

function q = queue_push_matlab(q, value, queue_size)
    if ~queue_is_full_matlab(q, queue_size)
        q.data(q.tail) = value;
        q.tail = mod(q.tail, queue_size) + 1;
        q.count = q.count + 1;
    else
        error('Queue overflow');
    end
end

function [q, value] = queue_pop_matlab(q, queue_size)
    if ~queue_is_empty_matlab(q)
        value = q.data(q.head);
        q.head = mod(q.head, queue_size) + 1;
        q.count = q.count - 1;
    else
        error('Queue underflow');
    end
end
