
function main()
    program = str2double(strsplit(fileread('input.txt'), ','));

    grid = containers.Map('KeyType', 'char', 'ValueType', 'double');
    robot_pos = [0, 0];
    robot_dir = 0;
    moves = [0, -1; 1, 0; 0, 1; -1, 0];

    memory = program;
    ip = 1;
    input_queue = [];
    output_queue = [];
    halted = false;

    while ~halted
        key = sprintf('%d,%d', robot_pos(1), robot_pos(2));
        if isKey(grid, key)
            current_color = grid(key);
        else
            current_color = 0;
        end

        input_queue(end+1) = current_color;
        run_computer();

        if numel(output_queue) == 2
            grid(key) = output_queue(1);
            
            if output_queue(2) == 0
                robot_dir = mod(robot_dir - 1, 4);
            else
                robot_dir = mod(robot_dir + 1, 4);
            end
            
            robot_pos = robot_pos + moves(robot_dir + 1, :);
            output_queue = [];
        end
    end

    fprintf('%d\n', grid.Count);

    function ensure_mem(addr)
        if addr > numel(memory)
            memory(addr) = 0;
        end
    end

    function val = get_param_val(offset, modes)
        mode = modes(offset);
        param_ptr = ip + offset;
        ensure_mem(param_ptr);
        switch mode
            case 0
                addr = memory(param_ptr) + 1;
                ensure_mem(addr);
                val = memory(addr);
            case 1
                val = memory(param_ptr);
        end
    end

    function addr = get_write_addr(offset)
        param_ptr = ip + offset;
        ensure_mem(param_ptr);
        addr = memory(param_ptr) + 1;
    end

    function run_computer()
        while true
            ensure_mem(ip);
            instruction = memory(ip);
            opcode = mod(instruction, 100);
            
            s = sprintf('%05d', instruction);
            modes = [str2double(s(3)), str2double(s(2)), str2double(s(1))];

            if opcode == 99
                halted = true;
                return;
            end

            switch opcode
                case {1, 2, 7, 8}
                    val1 = get_param_val(1, modes);
                    val2 = get_param_val(2, modes);
                    addr = get_write_addr(3);
                    ensure_mem(addr);
                    if opcode == 1, memory(addr) = val1 + val2;
                    elseif opcode == 2, memory(addr) = val1 * val2;
                    elseif opcode == 7, memory(addr) = double(val1 < val2);
                    else, memory(addr) = double(val1 == val2);
                    end
                    ip = ip + 4;
                case 3
                    if isempty(input_queue)
                        return;
                    end
                    addr = get_write_addr(1);
                    ensure_mem(addr);
                    memory(addr) = input_queue(1);
                    input_queue(1) = [];
                    ip = ip + 2;
                case 4
                    val = get_param_val(1, modes);
                    output_queue(end+1) = val;
                    ip = ip + 2;
                case {5, 6}
                    val = get_param_val(1, modes);
                    target = get_param_val(2, modes);
                    if (opcode == 5 && val ~= 0) || (opcode == 6 && val == 0)
                        ip = target + 1;
                    else
                        ip = ip + 3;
                    end
                otherwise
                    error('Unknown opcode');
            end
        end
    end
end
