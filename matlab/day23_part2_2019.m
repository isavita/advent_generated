
function advent_of_code_day23()
    % Constants
    NUM_COMPUTERS = 50;
    INITIAL_MEM_CAPACITY = 4096;
    QUEUE_CAPACITY = 1024;

    % Read input program
    fid = fopen('input.txt', 'r');
    if fid == -1
        error('Error opening input.txt');
    end
    program_str = fgetl(fid);
    fclose(fid);
    initial_program = str2double(strsplit(program_str, ','));

    % Initialize computers
    computers(NUM_COMPUTERS) = struct();
    packet_queues(NUM_COMPUTERS) = struct();

    for i = 1:NUM_COMPUTERS
        mem_cap = max(INITIAL_MEM_CAPACITY, length(initial_program) * 2);
        computers(i) = init_computer(initial_program, mem_cap, QUEUE_CAPACITY);
        packet_queues(i) = init_queue(QUEUE_CAPACITY);

        % Provide initial address (0-indexed)
        [computers(i).inputs, ~] = enqueue(computers(i).inputs, i - 1);
    end

    nat_x = -1;
    nat_y = -1;
    prev_nat_y = -2;
    nat_has_packet = false;

    while true
        network_was_idle = true;

        for i = 1:NUM_COMPUTERS
            if computers(i).halted, continue; end

            % Provide input from packet queue or -1
            if is_empty_queue(packet_queues(i))
                if is_empty_queue(computers(i).inputs)
                    [computers(i).inputs, ~] = enqueue(computers(i).inputs, -1);
                end
            else
                network_was_idle = false;
                [packet_queues(i), x, ~] = dequeue(packet_queues(i));
                [packet_queues(i), y, ~] = dequeue(packet_queues(i));
                [computers(i).inputs, ~] = enqueue(computers(i).inputs, x);
                [computers(i).inputs, ~] = enqueue(computers(i).inputs, y);
            end

            computers(i) = run_computer(computers(i));

            if ~computers(i).idle
                network_was_idle = false;
            end

            % Process outputs
            while computers(i).outputs.size >= 3
                network_was_idle = false;
                [computers(i).outputs, dest, ~] = dequeue(computers(i).outputs);
                [computers(i).outputs, x, ~] = dequeue(computers(i).outputs);
                [computers(i).outputs, y, ~] = dequeue(computers(i).outputs);

                if dest == 255
                    nat_x = x;
                    nat_y = y;
                    nat_has_packet = true;
                elseif dest >= 0 && dest < NUM_COMPUTERS
                    [packet_queues(dest + 1), ~] = enqueue(packet_queues(dest + 1), x);
                    [packet_queues(dest + 1), ~] = enqueue(packet_queues(dest + 1), y);
                end
            end
        end

        % Check for network idle state
        all_queues_empty_now = true;
        for i = 1:NUM_COMPUTERS
            if ~is_empty_queue(packet_queues(i))
                all_queues_empty_now = false;
                break;
            end
        end

        all_computers_idle_now = true;
        for i = 1:NUM_COMPUTERS
            if ~computers(i).halted && ~computers(i).idle
                all_computers_idle_now = false;
                break;
            end
        end

        if network_was_idle && all_queues_empty_now && all_computers_idle_now
            if nat_has_packet
                if nat_y == prev_nat_y
                    fprintf('%d\n', nat_y);
                    return;
                end
                prev_nat_y = nat_y;
                [packet_queues(1), ~] = enqueue(packet_queues(1), nat_x);
                [packet_queues(1), ~] = enqueue(packet_queues(1), nat_y);
                computers(1).idle = false;
                nat_has_packet = false;
            else
                all_halted = true;
                for i = 1:NUM_COMPUTERS
                    if ~computers(i).halted
                        all_halted = false;
                        break;
                    end
                end
                if all_halted
                    return;
                end
            end
        end
    end
end

% --- Queue Implementation ---
function q = init_queue(capacity)
    q.buffer = zeros(1, capacity);
    q.front = 1;
    q.rear = 0;
    q.size = 0;
    q.capacity = capacity;
end

function [q, success] = enqueue(q, value)
    success = true;
    if q.size >= q.capacity
        success = false;
        return;
    end
    q.rear = mod(q.rear, q.capacity) + 1;
    q.buffer(q.rear) = value;
    q.size = q.size + 1;
end

function [q, value, success] = dequeue(q)
    value = 0;
    success = true;
    if q.size == 0
        success = false;
        return;
    end
    value = q.buffer(q.front);
    q.front = mod(q.front, q.capacity) + 1;
    q.size = q.size - 1;
end

function empty = is_empty_queue(q)
    empty = (q.size == 0);
end

% --- Intcode Computer ---
function comp = init_computer(program, mem_capacity, queue_capacity)
    comp.memory = zeros(1, mem_capacity);
    comp.memory(1:length(program)) = program;
    comp.ip = 1;
    comp.relative_base = 0;
    comp.inputs = init_queue(queue_capacity);
    comp.outputs = init_queue(queue_capacity);
    comp.halted = false;
    comp.idle = false;
end

function val = mem_get(comp, address)
    if address < 0
        error('Negative memory address access: %d', address);
    end
    val = comp.memory(address + 1);
end

function comp = mem_set(comp, address, value)
    if address < 0
        error('Negative memory address write: %d', address);
    end
    comp.memory(address + 1) = value;
end

function addr_0_based = get_param_addr(comp, mode, offset)
    param_val_at_addr = mem_get(comp, comp.ip + offset - 1);
    switch mode
        case 0
            addr_0_based = param_val_at_addr;
        case 2
            addr_0_based = comp.relative_base + param_val_at_addr;
        otherwise
            error('Invalid mode for get_param_addr: %d', mode);
    end
end

function val = get_param(comp, mode, offset)
    param_val_at_addr = mem_get(comp, comp.ip + offset - 1);
    switch mode
        case 0
            val = mem_get(comp, param_val_at_addr);
        case 1
            val = param_val_at_addr;
        case 2
            val = mem_get(comp, comp.relative_base + param_val_at_addr);
        otherwise
            error('Unknown parameter mode: %d', mode);
    end
end

function comp = run_computer(comp)
    comp.idle = false;

    while true
        instruction = mem_get(comp, comp.ip - 1);
        opcode = mod(instruction, 100);
        modes = [floor(mod(instruction / 100, 10)), ...
                 floor(mod(instruction / 1000, 10)), ...
                 floor(mod(instruction / 10000, 10))];

        if opcode == 99
            comp.halted = true;
            break;
        end

        switch opcode
            case {1, 2, 7, 8}
                p1 = get_param(comp, modes(1), 1);
                p2 = get_param(comp, modes(2), 2);
                addr = get_param_addr(comp, modes(3), 3);
                
                result = 0;
                if opcode == 1, result = p1 + p2;
                elseif opcode == 2, result = p1 * p2;
                elseif opcode == 7, result = (p1 < p2);
                else, result = (p1 == p2);
                end
                comp = mem_set(comp, addr, result);
                comp.ip = comp.ip + 4;
            case 3
                addr = get_param_addr(comp, modes(1), 1);
                [comp.inputs, input_val, success] = dequeue(comp.inputs);
                if ~success
                    input_val = -1;
                    comp.idle = true;
                    comp = mem_set(comp, addr, input_val);
                    comp.ip = comp.ip + 2;
                    return;
                end

                if input_val == -1
                    comp.idle = true;
                else
                    comp.idle = false;
                end
                comp = mem_set(comp, addr, input_val);
                comp.ip = comp.ip + 2;

                if input_val == -1 && is_empty_queue(comp.inputs)
                    return;
                end

            case 4
                p1 = get_param(comp, modes(1), 1);
                [comp.outputs, success] = enqueue(comp.outputs, p1);
                if ~success
                    error('Output queue full!');
                end
                comp.ip = comp.ip + 2;
                comp.idle = false;
                if comp.outputs.size >= 3
                    return;
                end
            case {5, 6}
                p1 = get_param(comp, modes(1), 1);
                p2 = get_param(comp, modes(2), 2);
                if (opcode == 5 && p1 ~= 0) || (opcode == 6 && p1 == 0)
                    comp.ip = p2 + 1;
                else
                    comp.ip = comp.ip + 3;
                end
            case 9
                p1 = get_param(comp, modes(1), 1);
                comp.relative_base = comp.relative_base + p1;
                comp.ip = comp.ip + 2;
            otherwise
                error('Unknown opcode %d at ip %d', opcode, comp.ip - 1);
                comp.halted = true;
                return;
        end
    end
end
