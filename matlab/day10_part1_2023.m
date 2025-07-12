
function main()
    input_file = 'input.txt';
    fid = fopen(input_file, 'r');
    if fid == -1
        error('Error opening file: %s', input_file);
    end
    grid_data = textscan(fid, '%s', 'Delimiter', '\n');
    fclose(fid);

    grid_data = grid_data{1};
    height = length(grid_data);
    width = length(grid_data{1});

    grid = repmat('.', height, width);
    start_pos = [0, 0];

    for r = 1:height
        for c = 1:width
            char_val = grid_data{r}(c);
            grid(r, c) = char_val;
            if char_val == 'S'
                start_pos = [r, c];
            end
        end
    end

    path = zeros(height * width, 2);
    path(1, :) = start_pos;
    path_len = 1;

    current_pos = start_pos;
    prev_dir = [0, 0];

    possible_moves = struct('N', [-1, 0], 'E', [0, 1], 'S', [1, 0], 'W', [0, -1]);
    pipe_connections = struct(...
        '|', struct('N', true, 'S', true), ...
        '-', struct('E', true, 'W', true), ...
        'L', struct('N', true, 'E', true), ...
        'J', struct('N', true, 'W', true), ...
        '7', struct('S', true, 'W', true), ...
        'F', struct('S', true, 'E', true), ...
        '.', struct(), ...
        'S', struct('N', true, 'E', true, 'S', true, 'W', true) ...
    );

    % Determine initial move from 'S'
    for field = fieldnames(possible_moves)'
        dir_name = field{1};
        dir = possible_moves.(dir_name);
        next_r = current_pos(1) + dir(1);
        next_c = current_pos(2) + dir(2);

        if next_r >= 1 && next_r <= height && next_c >= 1 && next_c <= width
            next_char = grid(next_r, next_c);
            if isfield(pipe_connections, next_char)
                connections = pipe_connections.(next_char);
                if (strcmp(dir_name, 'N') && isfield(connections, 'S') && connections.S) || ...
                   (strcmp(dir_name, 'E') && isfield(connections, 'W') && connections.W) || ...
                   (strcmp(dir_name, 'S') && isfield(connections, 'N') && connections.N) || ...
                   (strcmp(dir_name, 'W') && isfield(connections, 'E') && connections.E)
                    current_pos = [next_r, next_c];
                    prev_dir = dir;
                    break;
                end
            end
        end
    end

    % Follow the path
    while ~isequal(current_pos, start_pos)
        path_len = path_len + 1;
        path(path_len, :) = current_pos;
        current_char = grid(current_pos(1), current_pos(2));
        connections = pipe_connections.(current_char);

        if isfield(connections, 'N') && connections.N && ~isequal(prev_dir, possible_moves.S)
            current_pos(1) = current_pos(1) + possible_moves.N(1);
            current_pos(2) = current_pos(2) + possible_moves.N(2);
            prev_dir = possible_moves.N;
        elseif isfield(connections, 'E') && connections.E && ~isequal(prev_dir, possible_moves.W)
            current_pos(1) = current_pos(1) + possible_moves.E(1);
            current_pos(2) = current_pos(2) + possible_moves.E(2);
            prev_dir = possible_moves.E;
        elseif isfield(connections, 'S') && connections.S && ~isequal(prev_dir, possible_moves.N)
            current_pos(1) = current_pos(1) + possible_moves.S(1);
            current_pos(2) = current_pos(2) + possible_moves.S(2);
            prev_dir = possible_moves.S;
        elseif isfield(connections, 'W') && connections.W && ~isequal(prev_dir, possible_moves.E)
            current_pos(1) = current_pos(1) + possible_moves.W(1);
            current_pos(2) = current_pos(2) + possible_moves.W(2);
            prev_dir = possible_moves.W;
        end
    end

    disp(floor(path_len / 2));
end
