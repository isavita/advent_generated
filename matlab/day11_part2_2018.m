
function main()
    % Read the grid serial number from input.txt
    serial_number = read_serial_number('input.txt');

    % Define grid dimensions
    GRID_SIZE = 300;

    % Calculate power levels for all cells
    power_grid = calculate_power_grid(serial_number, GRID_SIZE);

    % --- Part One: Find the largest 3x3 square ---
    [max_power_3x3, best_x_3x3, best_y_3x3] = find_largest_square(power_grid, 3);
    fprintf('Part One: Top-left corner of the largest 3x3 square: %d,%d (Total Power: %d)\n', best_x_3x3, best_y_3x3, max_power_3x3);

    % --- Part Two: Find the largest square of any size ---
    [max_power_any_size, best_x_any_size, best_y_any_size, best_size] = find_largest_square_any_size(power_grid, GRID_SIZE);
    fprintf('Part Two: Identifier of the largest square of any size: %d,%d,%d (Total Power: %d)\n', best_x_any_size, best_y_any_size, best_size, max_power_any_size);
end

function serial_number = read_serial_number(filename)
    % Reads the serial number from the first line of the specified file.
    try
        fid = fopen(filename, 'r');
        if fid == -1
            error('Could not open file: %s', filename);
        end
        serial_number_str = fgetl(fid);
        fclose(fid);
        serial_number = str2double(serial_number_str);
        if isnan(serial_number)
            error('Invalid serial number format in file: %s', filename);
        end
    catch ME
        fprintf('Error reading serial number: %s\n', ME.message);
        rethrow(ME);
    end
end

function power_grid = calculate_power_grid(serial_number, grid_size)
    % Calculates the power level for each fuel cell in the grid.
    power_grid = zeros(grid_size, grid_size);
    for y = 1:grid_size
        for x = 1:grid_size
            power_grid(y, x) = calculate_cell_power(x, y, serial_number);
        end
    end
end

function power_level = calculate_cell_power(x, y, serial_number)
    % Calculates the power level for a single fuel cell.
    rack_id = x + 10;
    power_level = (rack_id * y + serial_number) * rack_id;
    % Keep only the hundreds digit
    power_level = floor(mod(power_level, 1000) / 100);
    % Subtract 5
    power_level = power_level - 5;
end

function [max_power, best_x, best_y] = find_largest_square(power_grid, square_size)
    % Finds the top-left corner of the largest square of a given size.
    grid_size = size(power_grid, 1);
    max_power = -inf;
    best_x = -1;
    best_y = -1;

    % Pre-calculate sums for efficiency (integral image / summed-area table)
    % This is a common optimization for this type of problem.
    % We'll use a slightly modified approach to directly calculate sums of squares.
    % For a fixed square_size, we can iterate through possible top-left corners.

    for y = 1:(grid_size - square_size + 1)
        for x = 1:(grid_size - square_size + 1)
            current_power = sum(sum(power_grid(y : y + square_size - 1, x : x + square_size - 1)));
            if current_power > max_power
                max_power = current_power;
                best_x = x;
                best_y = y;
            end
        end
    end
end

function [max_power, best_x, best_y, best_size] = find_largest_square_any_size(power_grid, max_grid_size)
    % Finds the identifier (x, y, size) of the largest square of any size.
    grid_size = size(power_grid, 1);
    max_power = -inf;
    best_x = -1;
    best_y = -1;
    best_size = -1;

    % Optimization: Use a summed-area table (integral image) to quickly
    % calculate the sum of any rectangular region.
    % The integral image `ii` stores at `ii(r, c)` the sum of all elements
    % in the rectangle from (1,1) to (r,c) in the original `power_grid`.
    ii = zeros(grid_size + 1, grid_size + 1);
    for r = 1:grid_size
        for c = 1:grid_size
            ii(r + 1, c + 1) = power_grid(r, c) + ii(r, c + 1) + ii(r + 1, c) - ii(r, c);
        end
    end

    % Function to get the sum of a square region using the integral image
    get_square_sum = @(r, c, s) ii(r + s, c + s) - ii(r, c + s) - ii(r + s, c) + ii(r, c);

    % Iterate through all possible top-left corners and all possible sizes
    for y = 1:grid_size
        for x = 1:grid_size
            % For each top-left corner, iterate through possible square sizes
            % The maximum possible size for a square starting at (x,y) is
            % min(grid_size - x + 1, grid_size - y + 1).
            for s = 1:min(grid_size - x + 1, grid_size - y + 1)
                current_power = get_square_sum(y, x, s);
                if current_power > max_power
                    max_power = current_power;
                    best_x = x;
                    best_y = y;
                    best_size = s;
                end
            end
        end
    end
end

% Call the main function to execute the program
main();
