
function main()
    program = str2double(strsplit(fileread('input.txt'), ','));
    
    y = 20;
    x = 0;

    while true
        if ~check_beam(x, y, program)
            x = x + 1;
            continue;
        end

        if ~check_beam(x + 99, y, program)
            y = y + 1;
            continue;
        end
        
        if ~check_beam(x, y + 99, program)
            x = x + 1;
            continue;
        end
        
        fprintf('%d\n', x * 10000 + y);
        break;
    end
end

function is_in_beam = check_beam(x, y, program)
    output = run_intcode(program, [x, y]);
    if isempty(output)
        is_in_beam = false;
    else
        is_in_beam = (output == 1);
    end
end

function output = run_intcode(program, input_args)
    mem = zeros(1, 8192);
    mem(1:numel(program)) = program;
    ip = 1;
    relative_base = 0;
    input_idx = 1;
    output = [];

    while mem(ip) ~= 99
        instr = mem(ip);
        opcode = mod(instr, 100);
        
        m1 = mod(floor(instr/100), 10);
        m2 = mod(floor(instr/1000), 10);
        m3 = mod(floor(instr/10000), 10);

        p1 = 0; p2 = 0;
        a1 = 0; a3 = 0;

        if m1 == 0, p1_addr = mem(ip+1)+1; p1 = mem(p1_addr); a1 = p1_addr;
        elseif m1 == 1, p1 = mem(ip+1);
        else, p1_addr = mem(ip+1)+relative_base+1; p1 = mem(p1_addr); a1 = p1_addr;
        end

        if any(opcode == [1,2,5,6,7,8])
            if m2 == 0, p2_addr = mem(ip+2)+1; p2 = mem(p2_addr);
            elseif m2 == 1, p2 = mem(ip+2);
            else, p2_addr = mem(ip+2)+relative_base+1; p2 = mem(p2_addr);
            end
        end

        if any(opcode == [1,2,7,8])
            if m3 == 0, a3 = mem(ip+3)+1;
            else, a3 = mem(ip+3)+relative_base+1;
            end
        end

        switch opcode
            case 1
                mem(a3) = p1 + p2;
                ip = ip + 4;
            case 2
                mem(a3) = p1 * p2;
                ip = ip + 4;
            case 3
                mem(a1) = input_args(input_idx);
                input_idx = input_idx + 1;
                ip = ip + 2;
            case 4
                output = p1;
                return;
            case 5
                if p1 ~= 0, ip = p2 + 1; else, ip = ip + 3; end
            case 6
                if p1 == 0, ip = p2 + 1; else, ip = ip + 3; end
            case 7
                mem(a3) = p1 < p2;
                ip = ip + 4;
            case 8
                mem(a3) = p1 == p2;
                ip = ip + 4;
            case 9
                relative_base = relative_base + p1;
                ip = ip + 2;
        end
    end
end
