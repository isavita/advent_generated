
function main()
% This function serves as the main entry point for the program.

% Constants for coordinates
UNDEFINED = [0, 0];
TOP = [0, -1];
RIGHT = [1, 0];
BOTTOM = [0, 1];
LEFT = [-1, 0];

% Constants for tile characters
EMPTY = '.';
START = 'S';
VERTICAL = '|';
HORIZONTAL = '-';
TOP_LEFT_CORNER = 'J';
TOP_RIGHT_CORNER = 'L';
BOTTOM_LEFT_CORNER = '7';
BOTTOM_RIGHT_CORNER = 'F';

% Constants for pipe configurations (logical arrays: [top, right, bottom, left])
VERTICAL_PIPE = [true, false, true, false];
HORIZONTAL_PIPE = [false, true, false, true];
TOP_LEFT_CORNER_PIPE = [true, false, false, true];
TOP_RIGHT_CORNER_PIPE = [true, true, false, false];
BOTTOM_LEFT_CORNER_PIPE = [false, false, true, true];
BOTTOM_RIGHT_CORNER_PIPE = [false, true, true, false];

% Read input from file
fid = fopen('input.txt', 'r');
if fid == -1
    error('Error opening file input.txt');
end
input_lines = textscan(fid, '%s', 'Delimiter', '\n', 'Whitespace', '');
input_lines = input_lines{1}; % textscan returns a cell array within a cell
fclose(fid);

% Build grid (char matrix) from input lines
grid = char(input_lines);

% Find the starting coordinate 'S'
start_coord = find_start(grid);

% Find the path of the main loop
[path, ~] = path_finding(start_coord, grid);

% Create a new grid containing only the main loop path
path_grid = get_path_grid(grid, path, EMPTY);

% Count enclosed cells
cnt = 0;
[height, width] = size(path_grid);
for y = 1:height
    for x = 1:width
        c = [x, y]; % MATLAB uses (col, row) for (x,y)
        if is_inside(c, path_grid, EMPTY)
            cnt = cnt + 1;
        end
    end
end

% Print the result
fprintf('%d\n', cnt);

% --- Local Functions ---

    function c3 = coord_add(c1, c2)
        c3 = c1 + c2;
    end

    function c_opp = coord_opposite(c)
        c_opp = -c;
    end

    function pipe = get_pipe_from_tile(tile)
        switch tile
            case VERTICAL
                pipe = VERTICAL_PIPE;
            case HORIZONTAL
                pipe = HORIZONTAL_PIPE;
            case TOP_LEFT_CORNER
                pipe = TOP_LEFT_CORNER_PIPE;
            case TOP_RIGHT_CORNER
                pipe = TOP_RIGHT_CORNER_PIPE;
            case BOTTOM_LEFT_CORNER
                pipe = BOTTOM_LEFT_CORNER_PIPE;
            case BOTTOM_RIGHT_CORNER
                pipe = BOTTOM_RIGHT_CORNER_PIPE;
            otherwise
                pipe = [false, false, false, false];
        end
    end

    function tile = get_tile_from_pipe(pipe)
        if isequal(pipe, VERTICAL_PIPE)
            tile = VERTICAL;
        elseif isequal(pipe, HORIZONTAL_PIPE)
            tile = HORIZONTAL;
        elseif isequal(pipe, TOP_LEFT_CORNER_PIPE)
            tile = TOP_LEFT_CORNER;
        elseif isequal(pipe, TOP_RIGHT_CORNER_PIPE)
            tile = TOP_RIGHT_CORNER;
        elseif isequal(pipe, BOTTOM_LEFT_CORNER_PIPE)
            tile = BOTTOM_LEFT_CORNER;
        elseif isequal(pipe, BOTTOM_RIGHT_CORNER_PIPE)
            tile = BOTTOM_RIGHT_CORNER;
        else
            tile = EMPTY;
        end
    end

    function start_coord = find_start(grid)
        [~, c] = find(grid == START, 1); % Find first occurrence of 'S'
        [r, ~] = find(grid == START, 1);
        if isempty(r)
            start_coord = UNDEFINED;
        else
            start_coord = [c, r]; % MATLAB: [col, row] for (x,y)
        end
    end

    function pipe = get_pipe_from_neighbors(c, grid)
        pipe = [false, false, false, false];
        [height, width] = size(grid);

        % Directions: TOP, RIGHT, BOTTOM, LEFT
        dirs = {TOP, RIGHT, BOTTOM, LEFT};
        % Corresponding pipe indices: 1=top, 2=right, 3=bottom, 4=left
        pipe_indices = [1, 2, 3, 4];

        for i = 1:4
            dir = dirs{i};
            neighbor_coord = coord_add(c, dir);

            nx = neighbor_coord(1); % x-coordinate of neighbor
            ny = neighbor_coord(2); % y-coordinate of neighbor

            % Check if neighbor is within grid bounds
            if nx >= 1 && nx <= width && ny >= 1 && ny <= height
                neighbor_tile = grid(ny, nx); % Access grid using (row, col) -> (y, x)
                neighbor_pipe = get_pipe_from_tile(neighbor_tile);
                opp_dir = coord_opposite(dir); % Direction from neighbor back to current

                % Check if the neighbor's pipe connects back to the current cell
                if isequal(opp_dir, TOP) && neighbor_pipe(1) % Neighbor has a 'top' connection
                    pipe(pipe_indices(i)) = true;
                elseif isequal(opp_dir, RIGHT) && neighbor_pipe(2) % Neighbor has a 'right' connection
                    pipe(pipe_indices(i)) = true;
                elseif isequal(opp_dir, BOTTOM) && neighbor_pipe(3) % Neighbor has a 'bottom' connection
                    pipe(pipe_indices(i)) = true;
                elseif isequal(opp_dir, LEFT) && neighbor_pipe(4) % Neighbor has a 'left' connection
                    pipe(pipe_indices(i)) = true;
                end
            end
        end
    end

    function [path, path_len] = path_finding(start, grid)
        [height, width] = size(grid);
        % Preallocate path for maximum possible length (grid size)
        path = zeros(height * width, 2);
        path_index = 1;
        path(path_index, :) = start;

        start_pipe = get_pipe_from_neighbors(start, grid);
        previous_dir = UNDEFINED; % Placeholder, will be set
        current = UNDEFINED;

        % Determine initial direction from 'S'
        if start_pipe(1) % Connects to TOP
            previous_dir = TOP;
            current = coord_add(start, TOP);
        elseif start_pipe(2) % Connects to RIGHT
            previous_dir = RIGHT;
            current = coord_add(start, RIGHT);
        elseif start_pipe(3) % Connects to BOTTOM
            previous_dir = BOTTOM;
            current = coord_add(start, BOTTOM);
        else % Connects to LEFT
            previous_dir = LEFT;
            current = coord_add(start, LEFT);
        end

        % Traverse the loop until 'S' is reached again
        while ~isequal(current, start)
            path_index = path_index + 1;
            path(path_index, :) = current;

            current_tile = grid(current(2), current(1)); % Get tile at (y, x)
            current_pipe = get_pipe_from_tile(current_tile);

            % Determine next direction based on current pipe and previous direction
            if current_pipe(1) && ~isequal(previous_dir, BOTTOM) % Can go TOP and not coming from BOTTOM
                previous_dir = TOP;
                current = coord_add(current, TOP);
            elseif current_pipe(2) && ~isequal(previous_dir, LEFT) % Can go RIGHT and not coming from LEFT
                previous_dir = RIGHT;
                current = coord_add(current, RIGHT);
            elseif current_pipe(3) && ~isequal(previous_dir, TOP) % Can go BOTTOM and not coming from TOP
                previous_dir = BOTTOM;
                current = coord_add(current, BOTTOM);
            else % Must go LEFT and not coming from RIGHT
                previous_dir = LEFT;
                current = coord_add(current, LEFT);
            end
        end
        path_len = path_index;
        path = path(1:path_len, :); % Trim path to actual length
    end

    function path_grid = get_path_grid(grid, path, empty_char)
        [height, width] = size(grid);
        path_grid = repmat(empty_char, height, width); % Initialize with empty characters

        % Copy only the tiles that are part of the path
        for i = 1:size(path, 1)
            path_grid(path(i, 2), path(i, 1)) = grid(path(i, 2), path(i, 1));
        end

        % Replace 'S' with its actual pipe type for correct ray-casting
        start_coord = path(1, :); % Start is always the first element in path
        path_grid(start_coord(2), start_coord(1)) = get_tile_from_pipe(get_pipe_from_neighbors(start_coord, grid));
    end

    function inside = is_inside(c, grid, empty_char)
        % Check if the current cell is part of the path
        if grid(c(2), c(1)) ~= empty_char
            inside = false;
            return;
        end

        start_pipe_bend = empty_char; % Tracks the start of a horizontal bend (L or F)
        num_pipe_on_left = 0; % Counts crossings

        % Iterate from the current cell to the left edge
        for x = 1:(c(1) - 1)
            coord_left = [x, c(2)]; % Coordinate of the cell to the left
            v = grid(coord_left(2), coord_left(1)); % Get tile character

            switch v
                case VERTICAL
                    num_pipe_on_left = num_pipe_on_left + 1;
                case TOP_RIGHT_CORNER % 'L'
                    start_pipe_bend = TOP_RIGHT_CORNER;
                case BOTTOM_RIGHT_CORNER % 'F'
                    start_pipe_bend = BOTTOM_RIGHT_CORNER;
                case TOP_LEFT_CORNER % 'J'
                    if start_pipe_bend == BOTTOM_RIGHT_CORNER % F...J (counts as a vertical crossing)
                        start_pipe_bend = empty_char;
                        num_pipe_on_left = num_pipe_on_left + 1;
                    elseif start_pipe_bend == TOP_RIGHT_CORNER % L...J (U-turn, no crossing)
                        start_pipe_bend = empty_char;
                    end
                case BOTTOM_LEFT_CORNER % '7'
                    if start_pipe_bend == TOP_RIGHT_CORNER % L...7 (counts as a vertical crossing)
                        start_pipe_bend = empty_char;
                        num_pipe_on_left = num_pipe_on_left + 1;
                    elseif start_pipe_bend == BOTTOM_RIGHT_CORNER % F...7 (U-turn, no crossing)
                        start_pipe_bend = empty_char;
                    end
            end
        end
        % A cell is inside if it crosses an odd number of vertical pipes/segments
        inside = mod(num_pipe_on_left, 2) == 1;
    end
end
