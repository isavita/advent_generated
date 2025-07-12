
function solve()
    fid = fopen('input.txt', 'r');
    if fid == -1
        error('Error opening file input.txt');
    end

    total_sum = 0;

    while ~feof(fid)
        line = fgetl(fid);
        if ~ischar(line)
            break;
        end

        colon_idx = strfind(line, ':');
        if isempty(colon_idx)
            continue;
        end

        target_str = line(1:colon_idx-1);
        numbers_str = line(colon_idx+1:end);

        target_value = str2double(target_str);
        if isnan(target_value)
            continue;
        end

        numbers_cell = textscan(numbers_str, '%f', 'Delimiter', ' ', 'MultipleDelimsAsOne', true);
        numbers = numbers_cell{1};

        if ~isempty(numbers) && evaluate_expression(target_value, numbers)
            total_sum = total_sum + target_value;
        end
    end

    fclose(fid);
    fprintf('%d\n', total_sum);
end

function result = evaluate_expression(target, numbers)
    count = length(numbers);

    if count == 0
        result = false;
        return;
    end
    if count == 1
        result = (numbers(1) == target);
        return;
    end

    num_operators = count - 1;
    limit = 2^num_operators;

    for i = 0:(limit - 1)
        current_result = numbers(1);
        temp_i = i;

        for j = 1:num_operators
            op_type = bitand(temp_i, 1);
            temp_i = bitshift(temp_i, -1);

            if op_type == 0
                current_result = current_result + numbers(j + 1);
            else
                current_result = current_result * numbers(j + 1);
            end
        end

        if current_result == target
            result = true;
            return;
        end
    end

    result = false;
end

solve();
