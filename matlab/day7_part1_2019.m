
function main()
    original_code = str2double(strsplit(fileread('input.txt'), ','));
    phase_permutations = perms(0:4);
    max_signal = 0;

    for i = 1:size(phase_permutations, 1)
        phases = phase_permutations(i, :);
        signal = 0;
        for j = 1:5
            inputs = [phases(j), signal];
            signal = run_intcode(original_code, inputs);
        end
        max_signal = max(max_signal, signal);
    end

    fprintf('%d\n', max_signal);
end

function output = run_intcode(code, inputs)
    ip = 1;
    input_idx = 1;
    output = 0;

    function val = get_param(offset, mode)
        param_ptr = ip + offset;
        if mode == 1
            val = code(param_ptr);
        else
            val = code(code(param_ptr) + 1);
        end
    end

    while true
        instruction = code(ip);
        opcode = mod(instruction, 100);
        modes = floor(instruction / 100);
        mode1 = mod(modes, 10);
        mode2 = mod(floor(modes / 10), 10);

        switch opcode
            case 1
                p1 = get_param(1, mode1);
                p2 = get_param(2, mode2);
                addr = code(ip + 3) + 1;
                code(addr) = p1 + p2;
                ip = ip + 4;
            case 2
                p1 = get_param(1, mode1);
                p2 = get_param(2, mode2);
                addr = code(ip + 3) + 1;
                code(addr) = p1 * p2;
                ip = ip + 4;
            case 3
                addr = code(ip + 1) + 1;
                code(addr) = inputs(input_idx);
                input_idx = input_idx + 1;
                ip = ip + 2;
            case 4
                output = get_param(1, mode1);
                ip = ip + 2;
            case 5
                p1 = get_param(1, mode1);
                p2 = get_param(2, mode2);
                if p1 ~= 0
                    ip = p2 + 1;
                else
                    ip = ip + 3;
                end
            case 6
                p1 = get_param(1, mode1);
                p2 = get_param(2, mode2);
                if p1 == 0
                    ip = p2 + 1;
                else
                    ip = ip + 3;
                end
            case 7
                p1 = get_param(1, mode1);
                p2 = get_param(2, mode2);
                addr = code(ip + 3) + 1;
                code(addr) = p1 < p2;
                ip = ip + 4;
            case 8
                p1 = get_param(1, mode1);
                p2 = get_param(2, mode2);
                addr = code(ip + 3) + 1;
                code(addr) = p1 == p2;
                ip = ip + 4;
            case 99
                return;
        end
    end
end
