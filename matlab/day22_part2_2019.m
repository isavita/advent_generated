
function main()
    SIZE = uint64(119315717514047);
    ITERATIONS = uint64(101741582076661);
    TARGET_POS = uint64(2020);

    offset = uint64(0);
    increment = uint64(1);

    fid = fopen('input.txt', 'r');
    line = fgetl(fid);
    while ischar(line)
        if strcmp(line, 'deal into new stack')
            increment = mod_mul(increment, SIZE - 1, SIZE);
            offset = mod_add(offset, increment, SIZE);
        elseif startsWith(line, 'cut')
            n_val = sscanf(line, 'cut %f');
            n = uint64(mod(n_val, double(SIZE)));
            term = mod_mul(n, increment, SIZE);
            offset = mod_add(offset, term, SIZE);
        elseif startsWith(line, 'deal with increment')
            n = sscanf(line, 'deal with increment %f');
            inv_n = mod_inverse(uint64(n), SIZE);
            increment = mod_mul(increment, inv_n, SIZE);
        end
        line = fgetl(fid);
    end
    fclose(fid);

    final_increment = mod_pow(increment, ITERATIONS, SIZE);

    term1 = mod_sub(final_increment, uint64(1), SIZE);
    inc_minus_1 = mod_sub(increment, uint64(1), SIZE);
    term2_inv = mod_inverse(inc_minus_1, SIZE);
    
    final_offset = mod_mul(offset, term1, SIZE);
    final_offset = mod_mul(final_offset, term2_inv, SIZE);

    pos_times_inc = mod_mul(TARGET_POS, final_increment, SIZE);
    answer = mod_add(pos_times_inc, final_offset, SIZE);

    fprintf('%lu\n', answer);
end

function res = mod_add(a, b, m)
    res = mod(a + b, m);
end

function res = mod_sub(a, b, m)
    res = mod(a - b + m, m);
end

function res = mod_mul(a, b, m)
    res = uint64(0);
    a = mod(a, m);
    b = mod(b, m);
    while b > 0
        if mod(b, 2) == 1
            res = mod(res + a, m);
        end
        a = mod(a * 2, m);
        b = bitshift(b, -1);
    end
end

function res = mod_pow(base, exp, m)
    res = uint64(1);
    base = mod(base, m);
    while exp > 0
        if mod(exp, 2) == 1
            res = mod_mul(res, base, m);
        end
        base = mod_mul(base, base, m);
        exp = bitshift(exp, -1);
    end
end

function res = mod_inverse(n, m)
    res = mod_pow(n, m - 2, m);
end
