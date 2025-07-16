
function main()
    ops = {@addr, @addi, @mulr, @muli, @banr, @bani, @borr, @bori, ...
           @setr, @seti, @gtir, @gtri, @gtrr, @eqir, @eqri, @eqrr};

    txt = fileread('input.txt');
    parts = strsplit(txt, '\n\n\n');
    
    samples_str = parts{1};
    program_str = strtrim(parts{2});

    samples = sscanf(samples_str, 'Before: [%d, %d, %d, %d]\n%d %d %d %d\nAfter:  [%d, %d, %d, %d]\n\n');
    samples = reshape(samples, 12, [])';

    three_or_more = 0;
    possible_ops = true(16, 16);

    for i = 1:size(samples, 1)
        before = samples(i, 1:4);
        inst = samples(i, 5:8);
        after = samples(i, 9:12);
        
        opcode = inst(1) + 1;
        
        matching_ops = 0;
        for op_idx = 1:16
            res_reg = ops{op_idx}(before, inst(2), inst(3), inst(4));
            if isequal(res_reg, after)
                matching_ops = matching_ops + 1;
            else
                possible_ops(opcode, op_idx) = false;
            end
        end
        if matching_ops >= 3
            three_or_more = three_or_more + 1;
        end
    end
    fprintf('Part 1: %d\n', three_or_more);

    op_map = zeros(1, 16);
    while any(op_map == 0)
        for j = 1:16
            if op_map(j) == 0 && sum(possible_ops(j, :)) == 1
                op_idx = find(possible_ops(j, :));
                op_map(j) = op_idx;
                possible_ops(:, op_idx) = false;
            end
        end
    end

    program = sscanf(program_str, '%d %d %d %d');
    program = reshape(program, 4, [])';
    
    reg = [0, 0, 0, 0];
    for i = 1:size(program, 1)
        inst = program(i, :);
        opcode = inst(1) + 1;
        
        op_idx = op_map(opcode);
        reg = ops{op_idx}(reg, inst(2), inst(3), inst(4));
    end
    
    fprintf('Part 2: %d\n', reg(1));
end

function r = addr(r,a,b,c); r(c+1) = r(a+1) + r(b+1); end
function r = addi(r,a,b,c); r(c+1) = r(a+1) + b; end
function r = mulr(r,a,b,c); r(c+1) = r(a+1) * r(b+1); end
function r = muli(r,a,b,c); r(c+1) = r(a+1) * b; end
function r = banr(r,a,b,c); r(c+1) = bitand(r(a+1), r(b+1)); end
function r = bani(r,a,b,c); r(c+1) = bitand(r(a+1), b); end
function r = borr(r,a,b,c); r(c+1) = bitor(r(a+1), r(b+1)); end
function r = bori(r,a,b,c); r(c+1) = bitor(r(a+1), b); end
function r = setr(r,a,b,c); r(c+1) = r(a+1); end
function r = seti(r,a,b,c); r(c+1) = a; end
function r = gtir(r,a,b,c); r(c+1) = double(a > r(b+1)); end
function r = gtri(r,a,b,c); r(c+1) = double(r(a+1) > b); end
function r = gtrr(r,a,b,c); r(c+1) = double(r(a+1) > r(b+1)); end
function r = eqir(r,a,b,c); r(c+1) = double(a == r(b+1)); end
function r = eqri(r,a,b,c); r(c+1) = double(r(a+1) == b); end
function r = eqrr(r,a,b,c); r(c+1) = double(r(a+1) == r(b+1)); end
