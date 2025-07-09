
function main()
    size_x = 101;
    size_y = 103;
    
    fid = fopen('input.txt', 'r');
    if fid == -1
        error('Could not open input.txt');
    end
    
    robots = []; % Initialize as an empty matrix [x y vx vy]
    tline = fgetl(fid);
    while ischar(tline)
        if ~isempty(tline)
            [x, y, vx, vy] = parse_line(tline);
            robots = [robots; x, y, vx, vy]; %#ok<AGROW>
        end
        tline = fgetl(fid);
    end
    fclose(fid);

    % Part 1
    robots_part1 = robots;
    for i = 1:100
        robots_part1 = move_robots(robots_part1, size_x, size_y);
    end
    
    counts = count_quadrants(robots_part1, size_x, size_y);
    safety_factor = prod(counts);
    fprintf('Part 1 - Safety Factor after 100 seconds: %d\n', safety_factor);

    % Part 2
    robots_part2 = robots;
    seconds = 0;
    max_iterations = 1000000; 

    while true
        if has_no_overlaps(robots_part2)
            break;
        end
        robots_part2 = move_robots(robots_part2, size_x, size_y);
        seconds = seconds + 1;
        if seconds > max_iterations
            fprintf('Exceeded maximum iterations without finding a unique position configuration.\n');
            return;
        end
    end
    fprintf('Part 2 - Fewest seconds to display Easter egg: %d\n', seconds);
    fprintf('Final positions of robots:\n');
    draw_grid(robots_part2, size_x, size_y);
end

function [x, y, vx, vy] = parse_line(line)
    % p=(-?\d+),(-?\d+) v=(-?\d+),(-?\d+)
    tokens = regexp(line, 'p=(-?\d+),(-?\d+) v=(-?\d+),(-?\d+)', 'tokens', 'once');
    if isempty(tokens)
        error('Invalid line format: %s', line);
    end
    x = str2double(tokens{1});
    y = str2double(tokens{2});
    vx = str2double(tokens{3});
    vy = str2double(tokens{4});
end

function new_robots = move_robots(robots, size_x, size_y)
    % Extract current positions and velocities
    x = robots(:, 1);
    y = robots(:, 2);
    vx = robots(:, 3);
    vy = robots(:, 4);
    
    % Calculate new positions using vectorized operations and modulo
    new_x = mod(x + vx, size_x);
    new_y = mod(y + vy, size_y);
    
    % Reconstruct the new robots matrix
    new_robots = [new_x, new_y, vx, vy];
end

function counts = count_quadrants(robots, size_x, size_y)
    counts = zeros(1, 4);
    center_x = size_x / 2; % Use float division for comparison
    center_y = size_y / 2;
    
    x = robots(:, 1);
    y = robots(:, 2);
    
    % Quadrant 0: x < center_x, y < center_y
    counts(1) = sum(x < center_x & y < center_y);
    % Quadrant 1: x < center_x, y > center_y
    counts(2) = sum(x < center_x & y > center_y);
    % Quadrant 2: x > center_x, y < center_y
    counts(3) = sum(x > center_x & y < center_y);
    % Quadrant 3: x > center_x, y > center_y
    counts(4) = sum(x > center_x & y > center_y);
end

function result = has_no_overlaps(robots)
    positions = robots(:, 1:2); % Extract only x, y coordinates
    
    % Find unique rows (positions)
    unique_positions = unique(positions, 'rows');
    
    % If the number of unique positions equals the total number of robots,
    % then there are no overlaps.
    result = size(unique_positions, 1) == size(robots, 1);
end

function draw_grid(robots, size_x, size_y)
    grid = repmat('.', size_y, size_x); % Initialize grid with '.'
    
    % Convert 0-based coordinates to 1-based for MATLAB indexing
    x_coords = robots(:, 1) + 1;
    y_coords = robots(:, 2) + 1;
    
    % Place '#' at robot positions
    % Using sub2ind for direct linear indexing can be faster for many points
    idx = sub2ind(size(grid), y_coords, x_coords);
    grid(idx) = '#';
    
    % Print the grid row by row
    for i = 1:size_y
        fprintf('%s\n', grid(i, :));
    end
end
