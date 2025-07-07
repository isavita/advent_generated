
function main()
    % Read input from input.txt
    try
        fid = fopen('input.txt', 'r');
        if fid == -1
            error('Could not open input.txt for reading.');
        end
        map_cell = textscan(fid, '%s', 'Delimiter', '\n');
        fclose(fid);
    catch ME
        fprintf(2, 'Error reading input file: %s\n', ME.message);
        return;
    end

    map = map_cell{1};
    rows = length(map);
    cols = length(map{1});

    % Find starting position and direction
    start_row = -1;
    start_col = -1;
    start_dir = ''; % 'U', 'D', 'L', 'R'

    for r = 1:rows
        for c = 1:cols
            char_at_pos = map{r}(c);
            if char_at_pos == '^'
                start_row = r;
                start_col = c;
                start_dir = 'U';
                break;
            elseif char_at_pos == '>'
                start_row = r;
                start_col = c;
                start_dir = 'R';
                break;
            elseif char_at_pos == 'v'
                start_row = r;
                start_col = c;
                start_dir = 'D';
                break;
            elseif char_at_pos == '<'
                start_row = r;
                start_col = c;
                start_dir = 'L';
                break;
            end
        end
        if start_row ~= -1
            break;
        end
    end

    if start_row == -1
        fprintf(2, 'Error: Starting position (^) not found in the map.\n');
        return;
    end

    % Initialize visited positions and current state
    visited = containers.Map('KeyType', 'char', 'ValueType', 'logical');
    current_row = start_row;
    current_col = start_col;
    current_dir = start_dir;

    % Add starting position to visited
    visited_key = sprintf('%d,%d', current_row, current_col);
    visited(visited_key) = true;

    % Simulation loop
    while true
        % Determine position in front of the guard
        next_row = current_row;
        next_col = current_col;

        if current_dir == 'U'
            next_row = current_row - 1;
        elseif current_dir == 'D'
            next_row = current_row + 1;
        elseif current_dir == 'L'
            next_col = current_col - 1;
        elseif current_dir == 'R'
            next_col = current_col + 1;
        end

        % Check if the guard is out of bounds or hits an obstacle
        is_out_of_bounds = next_row < 1 || next_row > rows || next_col < 1 || next_col > cols;
        is_obstacle = false;
        if ~is_out_of_bounds
            char_in_front = map{next_row}(next_col);
            if char_in_front == '#'
                is_obstacle = true;
            end
        end

        if is_out_of_bounds || is_obstacle
            % Turn right 90 degrees
            if current_dir == 'U'
                current_dir = 'R';
            elseif current_dir == 'R'
                current_dir = 'D';
            elseif current_dir == 'D'
                current_dir = 'L';
            elseif current_dir == 'L'
                current_dir = 'U';
            end
        else
            % Take a step forward
            current_row = next_row;
            current_col = next_col;

            % Mark the new position as visited
            visited_key = sprintf('%d,%d', current_row, current_col);
            visited(visited_key) = true;
        end

        % Check if the guard has left the mapped area
        if is_out_of_bounds
            break;
        end
    end

    % Print the output
    fprintf('Number of distinct positions visited: %d\n', length(visited));

    % Optional: Print the map with visited positions marked
    output_map = cell(rows, 1);
    for r = 1:rows
        row_str = '';
        for c = 1:cols
            current_pos_key = sprintf('%d,%d', r, c);
            if isKey(visited, current_pos_key)
                row_str = [row_str, 'X'];
            else
                row_str = [row_str, map{r}(c)];
            end
        end
        output_map{r} = row_str;
    end

    fprintf('\nMap with visited positions:\n');
    for r = 1:rows
        fprintf('%s\n', output_map{r});
    end
end

% Entry point for the program
if nargin == 0
    main();
end
