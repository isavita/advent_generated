
function main()
    fid = fopen('input.txt', 'r');
    lines = textscan(fid, '%s', 'Delimiter', '\n', 'Whitespace', '');
    fclose(fid);
    
    input_grid_cell = lines{1};
    rows = numel(input_grid_cell);
    
    if rows == 0
        disp(0);
        return;
    end
    
    max_cols = 0;
    for i = 1:rows
        if length(input_grid_cell{i}) > max_cols
            max_cols = length(input_grid_cell{i});
        end
    end
    
    for i = 1:rows
        input_grid_cell{i} = [input_grid_cell{i}, repmat(' ', 1, max_cols - length(input_grid_cell{i}))];
    end
    
    grid_data = char(input_grid_cell);
    
    result = solve(grid_data);
    disp(result);
end

function load_val = solve(grid_data)
    North = [-1, 0]; 
    shifted_grid = shiftRocks(grid_data, North);
    load_val = calculateLoad(shifted_grid);
end

function grid = shiftRocks(grid, dir)
    [rows, cols] = size(grid);
    
    if (dir(1) == -1 && dir(2) == 0) || (dir(1) == 0 && dir(2) == -1)
        col_iter = 1:cols;
        row_iter = 1:rows;
    else
        col_iter = cols:-1:1;
        row_iter = rows:-1:1;
    end
    
    for c = col_iter
        for r = row_iter
            grid = shiftSingleRock(grid, [r, c], dir);
        end
    end
end

function grid = shiftSingleRock(grid, coord, dir)
    [rows, cols] = size(grid);
    
    current_r = coord(1);
    current_c = coord(2);
    
    if grid(current_r, current_c) == 'O'
        next_r = current_r + dir(1);
        next_c = current_c + dir(2);
        
        while isInBounds([next_r, next_c], rows, cols) && grid(next_r, next_c) == '.'
            grid(next_r, next_c) = 'O';
            grid(current_r, current_c) = '.';
            
            current_r = next_r;
            current_c = next_c;
            
            next_r = current_r + dir(1);
            next_c = current_c + dir(2);
        end
    end
end

function is_valid = isInBounds(coord, rows, cols)
    r = coord(1);
    c = coord(2);
    is_valid = r >= 1 && r <= rows && c >= 1 && c <= cols;
end

function load_val = calculateLoad(grid)
    [rows, ~] = size(grid);
    [r_indices, ~] = find(grid == 'O');
    load_val = sum(rows - r_indices + 1);
end
