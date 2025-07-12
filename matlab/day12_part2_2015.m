
function solution()
    filename = "input.txt";

    fid = fopen(filename, 'r');
    if fid == -1
        error('Error opening file: %s', filename);
    end
    file_content = fread(fid, '*char')';
    fclose(fid);

    if isempty(file_content)
        fprintf('Part 1 Sum: 0\n');
        fprintf('Part 2 Sum: 0\n');
        return;
    end

    numbers_str = regexp(file_content, '-?\d+', 'match');
    sum1 = sum(str2double(numbers_str));
    fprintf('Part 1 Sum: %d\n', sum1);

    json_data = jsondecode(file_content);
    sum2 = calculate_sum_part2_recursive(json_data);
    fprintf('Part 2 Sum: %d\n', sum2);
end

function current_sum = calculate_sum_part2_recursive(data)
    current_sum = 0;

    if isnumeric(data)
        current_sum = sum(data);
    elseif isstruct(data)
        field_names = fieldnames(data);
        contains_red_value = false;
        for i = 1:length(field_names)
            field_value = data.(field_names{i});
            if (ischar(field_value) && strcmp(field_value, 'red')) || ...
               (isstring(field_value) && field_value == "red")
                contains_red_value = true;
                break;
            end
        end

        if ~contains_red_value
            for i = 1:length(field_names)
                field_value = data.(field_names{i});
                current_sum = current_sum + calculate_sum_part2_recursive(field_value);
            end
        end
    elseif iscell(data)
        for i = 1:length(data)
            current_sum = current_sum + calculate_sum_part2_recursive(data{i});
        end
    end
end
