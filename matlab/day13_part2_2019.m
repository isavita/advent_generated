
function main()
    fid = fopen('input.txt', 'r');
    line = fgetl(fid);
    fclose(fid);
    program = int64(str2double(strsplit(line, ',')));

    memory = int64(zeros(1, 10000));
    memory(1:length(program)) = program;
    memory(1) = 2;

    ip = 1;
    relative_base = int64(0);
    halted = false;

    score = int64(0);
    ball_x = int64(0);
    paddle_x = int64(0);

    output_state = 0;
    temp_x = int64(0);
    temp_y = int64(0);

    while ~halted
        instruction = memory(ip);
        opcode = mod(instruction, 100);
        
        modes = [
            floor(mod(instruction, 1000) / 100), ...
            floor(mod(instruction, 10000) / 1000), ...
            floor(mod(instruction, 100000) / 10000)
        ];

        get_param = @(offset, mode) get_param_value(memory, ip, relative_base, offset, mode);
        get_addr = @(offset, mode) get_write_address(memory, ip, relative_base, offset, mode);

        switch opcode
            case {1, 2}
                p1 = get_param(1, modes(1));
                p2 = get_param(2, modes(2));
                addr = get_addr(3, modes(3));
                if opcode == 1
                    memory(addr) = p1 + p2;
                else
                    memory(addr) = p1 * p2;
                end
                ip = ip + 4;
            case 3
                addr = get_addr(1, modes(1));
                memory(addr) = sign(ball_x - paddle_x);
                ip = ip + 2;
            case 4
                output_val = get_param(1, modes(1));
                if output_state == 0
                    temp_x = output_val;
                    output_state = 1;
                elseif output_state == 1
                    temp_y = output_val;
                    output_state = 2;
                else
                    tile_id = output_val;
                    if temp_x == -1 && temp_y == 0
                        score = tile_id;
                    else
                        if tile_id == 3
                            paddle_x = temp_x;
                        elseif tile_id == 4
                            ball_x = temp_x;
                        end
                    end
                    output_state = 0;
                end
                ip = ip + 2;
            case {5, 6}
                p1 = get_param(1, modes(1));
                p2 = get_param(2, modes(2));
                if (opcode == 5 && p1 ~= 0) || (opcode == 6 && p1 == 0)
                    ip = p2 + 1;
                else
                    ip = ip + 3;
                end
            case {7, 8}
                p1 = get_param(1, modes(1));
                p2 = get_param(2, modes(2));
                addr = get_addr(3, modes(3));
                if (opcode == 7 && p1 < p2) || (opcode == 8 && p1 == p2)
                    memory(addr) = 1;
                else
                    memory(addr) = 0;
                end
                ip = ip + 4;
            case 9
                p1 = get_param(1, modes(1));
                relative_base = relative_base + p1;
                ip = ip + 2;
            case 99
                halted = true;
            otherwise
                halted = true;
        end
    end
    fprintf('%d\n', score);
end

function val = get_param_value(memory, ip, relative_base, offset, mode)
    switch mode
        case 0
            val = memory(memory(ip + offset) + 1);
        case 1
            val = memory(ip + offset);
        case 2
            val = memory(relative_base + memory(ip + offset) + 1);
    end
end

function addr = get_write_address(memory, ip, relative_base, offset, mode)
    if mode == 0
        addr = memory(ip + offset) + 1;
    else
        addr = relative_base + memory(ip + offset) + 1;
    end
end
