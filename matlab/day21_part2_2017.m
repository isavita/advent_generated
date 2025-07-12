
function main()
    rules = containers.Map('KeyType', 'char', 'ValueType', 'char');
    fid = fopen('input.txt', 'r');
    while ~feof(fid)
        line = strtrim(fgetl(fid));
        parts = strsplit(line, ' => ');
        rules(parts{1}) = parts{2};
    end
    fclose(fid);

    grid = {'.#.', '..#', '###'};

    for iter = 1:18
        if mod(length(grid), 2) == 0
            sub_size = 2;
            new_sub_size = 3;
        else
            sub_size = 3;
            new_sub_size = 4;
        end

        grid_size = length(grid);
        new_grid_size = (grid_size / sub_size) * new_sub_size;
        new_grid = cell(new_grid_size, 1);
        for i = 1:new_grid_size
            new_grid{i} = repmat('.', 1, new_grid_size);
        end

        for y = 0:(sub_size):(grid_size - 1)
            for x = 0:(sub_size):(grid_size - 1)
                sub_square_str = '';
                for dy = 0:(sub_size - 1)
                    sub_square_str = [sub_square_str, grid{y + dy + 1}(x + 1 : x + sub_size)]; %#ok<AGROW>
                    if dy < sub_size - 1
                        sub_square_str = [sub_square_str, '/']; %#ok<AGROW>
                    end
                end

                enhanced_square_str = enhance(sub_square_str, rules);

                new_y_base = floor(y / sub_size) * new_sub_size;
                new_x_base = floor(x / sub_size) * new_sub_size;
                tokens = strsplit(enhanced_square_str, '/');
                for dy = 0:(new_sub_size - 1)
                    new_grid{new_y_base + dy + 1}(new_x_base + 1 : new_x_base + new_sub_size) = tokens{dy + 1};
                end
            end
        end
        grid = new_grid;
    end

    count = 0;
    for i = 1:length(grid)
        count = count + sum(grid{i} == '#');
    end
    fprintf('%d\n', count);
end

function enhanced_str = enhance(input_str, rules)
    persistent memo;
    if isempty(memo)
        memo = containers.Map('KeyType', 'char', 'ValueType', 'char');
    end

    if isKey(memo, input_str)
        enhanced_str = memo(input_str);
        return;
    end

    current_str = input_str;
    for i = 1:4
        if isKey(rules, current_str)
            enhanced_str = rules(current_str);
            memo(input_str) = enhanced_str;
            return;
        end
        current_str = rotate_str(current_str);
    end

    current_str = flip_str(input_str);
    for i = 1:4
        if isKey(rules, current_str)
            enhanced_str = rules(current_str);
            memo(input_str) = enhanced_str;
            return;
        end
        current_str = rotate_str(current_str);
    end
    error('No rule found for pattern %s', input_str);
end

function rotated_str = rotate_str(input_str)
    if length(input_str) == 5
        size = 2;
    elseif length(input_str) == 11
        size = 3;
    else
        error('Invalid input string length for rotation');
    end

    output_str = '';
    for x = 0:(size - 1)
        for y = (size - 1):-1:0
            output_str = [output_str, input_str(y * (size + 1) + x + 1)]; %#ok<AGROW>
        end
        if x < size - 1
            output_str = [output_str, '/']; %#ok<AGROW>
        end
    end
    rotated_str = output_str;
end

function flipped_str = flip_str(input_str)
    if length(input_str) == 5
        size = 2;
    elseif length(input_str) == 11
        size = 3;
    else
        error('Invalid input string length for flip');
    end

    output_str_cell = cell(1, size);
    row_start = 1;
    for y = 0:(size - 1)
        row = input_str(row_start : row_start + size - 1);
        output_str_cell{y + 1} = row(end:-1:1);
        row_start = row_start + size + 1;
    end
    flipped_str = strjoin(output_str_cell, '/');
end
