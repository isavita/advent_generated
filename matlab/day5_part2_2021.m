
function main()
    % Read input from input.txt
    try
        fid = fopen('input.txt', 'r');
        if fid == -1
            error('Could not open input.txt for reading.');
        end
        lines = textscan(fid, '%d,%d -> %d,%d', 'Delimiter', ' ');
        fclose(fid);
    catch ME
        fprintf(2, 'Error reading input file: %s\n', ME.message);
        return;
    end

    % Initialize the grid. We need to determine the maximum coordinate to
    % size the grid appropriately. A common approach is to find the max
    % x and y from all lines. For this problem, the example suggests a 10x10
    % grid, but a more robust solution would dynamically determine this.
    % Let's assume a maximum coordinate of 1000 for generality, as the
    % problem doesn't specify an upper bound and the example is small.
    % A more efficient approach for sparse grids would be to use a map
    % or hash table, but for this problem, a 2D array is manageable.
    max_coord = 1000; % Assuming a reasonable maximum coordinate
    grid = zeros(max_coord + 1, max_coord + 1, 'int32');

    % Process each line segment
    num_lines = length(lines{1});
    for i = 1:num_lines
        x1 = lines{1}(i);
        y1 = lines{2}(i);
        x2 = lines{3}(i);
        y2 = lines{4}(i);

        % Update the grid based on the line segment
        grid = updateGrid(grid, x1, y1, x2, y2);
    end

    % Count points where at least two lines overlap
    overlap_count = countOverlaps(grid);

    % Print the output to standard output
    fprintf('Number of points where at least two lines overlap: %d\n', overlap_count);
end

function grid = updateGrid(grid, x1, y1, x2, y2)
    % This function updates the grid by incrementing the count for each
    % point covered by the line segment. It handles horizontal, vertical,
    % and 45-degree diagonal lines.

    dx = sign(x2 - x1);
    dy = sign(y2 - y1);

    % Determine the number of steps needed to draw the line
    % This is the maximum of the absolute differences in x and y coordinates.
    steps = max(abs(x2 - x1), abs(y2 - y1));

    % Iterate through each point on the line segment
    current_x = x1;
    current_y = y1;
    for k = 0:steps
        % Ensure coordinates are within grid bounds (though with max_coord=1000,
        % this is unlikely to be an issue for typical inputs).
        if current_x >= 0 && current_x < size(grid, 2) && ...
           current_y >= 0 && current_y < size(grid, 1)
            grid(current_y + 1, current_x + 1) = grid(current_y + 1, current_x + 1) + 1;
        end

        % Move to the next point on the line
        current_x = current_x + dx;
        current_y = current_y + dy;
    end
end

function overlap_count = countOverlaps(grid)
    % This function counts the number of cells in the grid that have a
    % value of 2 or greater.
    overlap_count = sum(grid(:) >= 2);
end

% To run this program, save it as a .m file (e.g., hydrothermal_venture.m)
% and then call the main function from the MATLAB command window:
%
% >> hydrothermal_venture
%
% Ensure that a file named 'input.txt' exists in the same directory
% as the MATLAB script, containing the vent data in the specified format.
