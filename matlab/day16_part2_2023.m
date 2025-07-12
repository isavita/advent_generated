
function main()
    grid = read_grid("input.txt");
    [height, width] = size(grid);

    max_energized = 0;

    % Top edge
    for x = 1:width
        max_energized = max(max_energized, simulate_beam(grid, x, 0, 0, 1));
    end

    % Bottom edge
    for x = 1:width
        max_energized = max(max_energized, simulate_beam(grid, x, height + 1, 0, -1));
    end

    % Left edge
    for y = 1:height
        max_energized = max(max_energized, simulate_beam(grid, 0, y, 1, 0));
    end

    % Right edge
    for y = 1:height
        max_energized = max(max_energized, simulate_beam(grid, width + 1, y, -1, 0));
    end

    fprintf('%d\n', max_energized);
end

function grid = read_grid(filename)
    fid = fopen(filename, 'r');
    if fid == -1
        error('Error opening input file');
    end
    grid_lines = textscan(fid, '%s', 'Delimiter', '\n');
    fclose(fid);
    grid = cell2mat(grid_lines{1});
end

function energized_count = simulate_beam(grid, start_x, start_y, start_dx, start_dy)
    [height, width] = size(grid);
    energized = false(height, width);
    visited = false(height, width, 4); % 0:R, 1:L, 2:D, 3:U

    queue = struct('x', {}, 'y', {}, 'dx', {}, 'dy', {});
    queue_head = 1;
    queue_tail = 1;

    queue(queue_tail) = struct('x', start_x, 'y', start_y, 'dx', start_dx, 'dy', start_dy);
    queue_tail = queue_tail + 1;

    while queue_head < queue_tail
        current = queue(queue_head);
        queue_head = queue_head + 1;

        x = current.x;
        y = current.y;
        dx = current.dx;
        dy = current.dy;

        nx = x + dx;
        ny = y + dy;

        if nx < 1 || nx > width || ny < 1 || ny > height
            continue;
        end

        dir_idx = dir_to_index(dx, dy);
        if visited(ny, nx, dir_idx)
            continue;
        end
        visited(ny, nx, dir_idx) = true;
        energized(ny, nx) = true;

        cell = grid(ny, nx);
        new_states = [];

        if cell == '.'
            new_states = [new_states, struct('x', nx, 'y', ny, 'dx', dx, 'dy', dy)];
        elseif cell == '/'
            new_states = [new_states, struct('x', nx, 'y', ny, 'dx', -dy, 'dy', -dx)];
        elseif cell == '\'
            new_states = [new_states, struct('x', nx, 'y', ny, 'dx', dy, 'dy', dx)];
        elseif cell == '|'
            if dx ~= 0
                new_states = [new_states, struct('x', nx, 'y', ny, 'dx', 0, 'dy', 1)];
                new_states = [new_states, struct('x', nx, 'y', ny, 'dx', 0, 'dy', -1)];
            else
                new_states = [new_states, struct('x', nx, 'y', ny, 'dx', dx, 'dy', dy)];
            end
        elseif cell == '-'
            if dy ~= 0
                new_states = [new_states, struct('x', nx, 'y', ny, 'dx', 1, 'dy', 0)];
                new_states = [new_states, struct('x', nx, 'y', ny, 'dx', -1, 'dy', 0)];
            else
                new_states = [new_states, struct('x', nx, 'y', ny, 'dx', dx, 'dy', dy)];
            end
        end

        for i = 1:length(new_states)
            queue(queue_tail) = new_states(i);
            queue_tail = queue_tail + 1;
        end
    end

    energized_count = sum(energized(:));
end

function index = dir_to_index(dx, dy)
    if dx == 1 && dy == 0
        index = 1;
    elseif dx == -1 && dy == 0
        index = 2;
    elseif dx == 0 && dy == 1
        index = 3;
    elseif dx == 0 && dy == -1
        index = 4;
    else
        index = 0; % Should not happen
    end
end
