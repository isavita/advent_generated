
function main()
    fid = fopen('input.txt', 'r');
    input_lines = textscan(fid, '%s', 'Delimiter', '\n', 'Whitespace', '');
    fclose(fid);
    input_lines = input_lines{1};

    height = numel(input_lines);
    width = length(input_lines{1});

    grid = char(zeros(height, width));
    for i = 1:height
        grid(i, :) = input_lines{i};
    end

    result = solve(grid);
    fprintf('%d\n', result);
end

function load_val = solve(initial_grid)
    numCycles = 1000000000;
    grid = initial_grid;
    cache = containers.Map('KeyType', 'char', 'ValueType', 'double');

    for i = 0:numCycles-1
        gridKey = grid(:)';

        if cache.isKey(gridKey)
            first_occurrence = cache(gridKey);
            cycle_length = i - first_occurrence;
            remainingCycles = mod(numCycles - first_occurrence, cycle_length);

            for j = 1:remainingCycles
                grid = cycleRocks(grid);
            end
            load_val = calculateLoad(grid);
            return;
        end

        cache(gridKey) = i;
        grid = cycleRocks(grid);
    end

    load_val = calculateLoad(grid);
end

function grid = cycleRocks(grid)
    grid = shiftRocks(grid, [-1, 0]); % North
    grid = shiftRocks(grid, [0, -1]); % West
    grid = shiftRocks(grid, [1, 0]);  % South
    grid = shiftRocks(grid, [0, 1]);  % East
end

function grid = shiftRocks(grid, dir)
    [height, width] = size(grid);
    newGrid = repmat('.', height, width);
    newGrid(grid == '#') = '#';

    if isequal(dir, [-1, 0]) % North (row decreases)
        for c = 1:width
            next_row = 1;
            for r = 1:height
                if grid(r, c) == '#'
                    next_row = r + 1;
                elseif grid(r, c) == 'O'
                    newGrid(next_row, c) = 'O';
                    next_row = next_row + 1;
                end
            end
        end
    elseif isequal(dir, [0, -1]) % West (col decreases)
        for r = 1:height
            next_col = 1;
            for c = 1:width
                if grid(r, c) == '#'
                    next_col = c + 1;
                elseif grid(r, c) == 'O'
                    newGrid(r, next_col) = 'O';
                    next_col = next_col + 1;
                end
            end
        end
    elseif isequal(dir, [1, 0]) % South (row increases)
        for c = 1:width
            next_row = height;
            for r = height:-1:1
                if grid(r, c) == '#'
                    next_row = r - 1;
                elseif grid(r, c) == 'O'
                    newGrid(next_row, c) = 'O';
                    next_row = next_row - 1;
                end
            end
        end
    elseif isequal(dir, [0, 1]) % East (col increases)
        for r = 1:height
            next_col = width;
            for c = width:-1:1
                if grid(r, c) == '#'
                    next_col = c - 1;
                elseif grid(r, c) == 'O'
                    newGrid(r, next_col) = 'O';
                    next_col = next_col - 1;
                end
            end
        end
    end
    grid = newGrid;
end

function load_val = calculateLoad(grid)
    [height, ~] = size(grid);
    [rows, ~] = find(grid == 'O');
    load_val = sum(height - rows + 1);
end

main();
