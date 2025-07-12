
function main()
    fid = fopen('input.txt', 'r');
    if fid == -1
        error('Error: Could not open input.txt');
    end

    total_part1 = int64(0);
    total_part2 = int64(0);

    tline = fgetl(fid);
    while ischar(tline)
        if ~isempty(tline)
            total_part1 = total_part1 + evaluate(tline, 1);
            total_part2 = total_part2 + evaluate(tline, 2);
        end
        tline = fgetl(fid);
    end

    fclose(fid);

    fprintf('%d\n', total_part1);
    fprintf('%d\n', total_part2);
end

function result = evaluate(expression, rule)
    num_stack = int64([]);
    op_stack = char([]);

    idx = 1;
    len_expr = length(expression);

    while idx <= len_expr
        char_val = expression(idx);

        if isspace(char_val)
            idx = idx + 1;
        elseif isdigit(char_val)
            num_start_idx = idx;
            while idx <= len_expr && isdigit(expression(idx))
                idx = idx + 1;
            end
            num_str = expression(num_start_idx : idx - 1);
            num_val = int64(str2double(num_str));
            num_stack = [num_stack, num_val];
        elseif char_val == '('
            op_stack = [op_stack, '('];
            idx = idx + 1;
        elseif char_val == ')'
            while ~isempty(op_stack) && op_stack(end) ~= '('
                [num_stack, op_stack] = apply_op(num_stack, op_stack);
            end
            if isempty(op_stack) || op_stack(end) ~= '('
                error('Mismatched parentheses');
            end
            op_stack(end) = [];
            idx = idx + 1;
        elseif char_val == '+' || char_val == '*'
            while ~isempty(op_stack) && op_stack(end) ~= '(' && ...
                  precedence(op_stack(end), rule) >= precedence(char_val, rule)
                [num_stack, op_stack] = apply_op(num_stack, op_stack);
            end
            op_stack = [op_stack, char_val];
            idx = idx + 1;
        else
            error('Invalid character: %c', char_val);
        end
    end

    while ~isempty(op_stack)
        if op_stack(end) == '('
            error('Mismatched parentheses at end');
        end
        [num_stack, op_stack] = apply_op(num_stack, op_stack);
    end

    if length(num_stack) ~= 1
        error('Invalid final number stack state');
    end
    result = num_stack(end);
end

function p = precedence(op, rule)
    if op == '('
        p = 0;
    elseif rule == 1
        p = 1;
    else
        if op == '+'
            p = 2;
        elseif op == '*'
            p = 1;
        else
            p = 0;
        end
    end
end

function [num_stack, op_stack] = apply_op(num_stack, op_stack)
    op = op_stack(end); op_stack(end) = [];
    right = num_stack(end); num_stack(end) = [];
    left = num_stack(end); num_stack(end) = [];

    if op == '+'
        result = left + right;
    elseif op == '*'
        result = left * right;
    else
        error('Unknown operator: %c', op);
    end
    num_stack = [num_stack, result];
end
