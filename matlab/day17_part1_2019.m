
function main()
    program = read_program('input.txt');
    result = run_program(program);
    fprintf('%d\n', result);
end

function program = read_program(filename)
    fid = fopen(filename, 'r');
    line = fgetl(fid);
    fclose(fid);
    program = str2num(['[', line, ']']);
end

function result = run_program(program)
    data = program;
    size_data = numel(data);
    ip = 1;
    relbase = 0;
    output_buffer = '';

    while true
        op_full = data(ip);
        op = mod(op_full, 100);
        modes = [mod(floor(op_full / 100), 10), mod(floor(op_full / 1000), 10), mod(floor(op_full / 10000), 10)];

        if op == 1
            val1 = get_value(data, ip + 1, modes(1), relbase);
            val2 = get_value(data, ip + 2, modes(2), relbase);
            data = set_value(data, ip + 3, modes(3), val1 + val2, relbase);
            ip = ip + 4;
        elseif op == 2
            val1 = get_value(data, ip + 1, modes(1), relbase);
            val2 = get_value(data, ip + 2, modes(2), relbase);
            data = set_value(data, ip + 3, modes(3), val1 * val2, relbase);
            ip = ip + 4;
        elseif op == 3
            data = set_value(data, ip + 1, modes(1), 0, relbase);
            ip = ip + 2;
        elseif op == 4
            output_buffer = [output_buffer, char(get_value(data, ip + 1, modes(1), relbase))];
            ip = ip + 2;
        elseif op == 5
            if get_value(data, ip + 1, modes(1), relbase) ~= 0
                ip = get_value(data, ip + 2, modes(2), relbase) + 1;
            else
                ip = ip + 3;
            end
        elseif op == 6
            if get_value(data, ip + 1, modes(1), relbase) == 0
                ip = get_value(data, ip + 2, modes(2), relbase) + 1;
            else
                ip = ip + 3;
            end
        elseif op == 7
            val1 = get_value(data, ip + 1, modes(1), relbase);
            val2 = get_value(data, ip + 2, modes(2), relbase);
            data = set_value(data, ip + 3, modes(3), val1 < val2, relbase);
            ip = ip + 4;
        elseif op == 8
            val1 = get_value(data, ip + 1, modes(1), relbase);
            val2 = get_value(data, ip + 2, modes(2), relbase);
            data = set_value(data, ip + 3, modes(3), val1 == val2, relbase);
            ip = ip + 4;
        elseif op == 9
            relbase = relbase + get_value(data, ip + 1, modes(1), relbase);
            ip = ip + 2;
        elseif op == 99
            break;
        end
    end

    grid = zeros(100, 100);
    x = 1;
    y = 1;
    for i = 1:numel(output_buffer)
        if output_buffer(i) == newline
            x = 1;
            y = y + 1;
        else
            if ismember(output_buffer(i), ['#', '^', 'v', '<', '>'])
                grid(y, x) = 1;
            end
            x = x + 1;
        end
    end

    sum_val = 0;
    for i = 2:98
        for j = 2:98
            if grid(i, j) && grid(i-1, j) && grid(i+1, j) && grid(i, j-1) && grid(i, j+1)
                sum_val = sum_val + (i-1) * (j-1);
            end
        end
    end
    result = sum_val;
end

function val = get_value(data, index, mode, relbase)
    if index > numel(data)
        val = 0;
        return;
    end
    if mode == 0
        if data(index) + 1 > numel(data)
            val = 0;
        else
            val = data(data(index) + 1);
        end
    elseif mode == 1
        val = data(index);
    elseif mode == 2
        if relbase + data(index) + 1 > numel(data)
            val = 0;
        else
            val = data(relbase + data(index) + 1);
        end
    end
end

function data = set_value(data, index, mode, val, relbase)
    if index > numel(data)
        data(index) = 0;
    end
    if mode == 0
        if data(index) + 1 > numel(data)
            data(data(index) + 1) = 0;
        end
        data(data(index) + 1) = val;
    elseif mode == 2
        if relbase + data(index) + 1 > numel(data)
            data(relbase + data(index) + 1) = 0;
        end
        data(relbase + data(index) + 1) = val;
    end
end

main();
