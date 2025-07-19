
function main()
    fid = fopen('input.txt', 'r');
    k = zeros(1, 14);
    l = zeros(1, 14);
    m = zeros(1, 14);
    i = 0;
    line = fgetl(fid);
    while ischar(line)
        idx = floor(i / 18) + 1;
        if mod(i, 18) == 4
            l(idx) = sscanf(line, 'div z %d');
        elseif mod(i, 18) == 5
            k(idx) = sscanf(line, 'add x %d');
        elseif mod(i, 18) == 15
            m(idx) = sscanf(line, 'add y %d');
        end
        line = fgetl(fid);
        i = i + 1;
    end
    fclose(fid);

    constraints = zeros(14, 2);
    stack = [];
    for i = 1:14
        if l(i) == 1
            stack(end+1) = i;
        else
            pop_idx = stack(end);
            stack(end) = [];
            constraints(pop_idx, 1) = i;
            constraints(pop_idx, 2) = m(pop_idx) + k(i);
        end
    end

    min_digits = zeros(1, 14);
    for i = 1:14
        if constraints(i, 1) ~= 0
            j = constraints(i, 1);
            diff = constraints(i, 2);
            w_i = max(1, 1 - diff);
            min_digits(i) = w_i;
            min_digits(j) = w_i + diff;
        end
    end

    fprintf('%d', min_digits);
    fprintf('\n');
end
