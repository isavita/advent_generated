
function main()
    % Read the topographic map from input.txt
    map = read_map('input.txt');
    [rows, cols] = size(map);

    total_rating = 0;

    % Iterate through each cell to find trailheads (height 0)
    for r = 1:rows
        for c = 1:cols
            if map(r, c) == 0
                % If it's a trailhead, calculate its rating
                rating = calculate_trailhead_rating(map, r, c);
                total_rating = total_rating + rating;
            end
        end
    end

    % Print the total sum of ratings to standard output
    fprintf('%d\n', total_rating);
end

function map = read_map(filename)
    % Reads the topographic map from a file.
    % Each line in the file represents a row of the map.
    % Characters are converted to their integer height values.
    fid = fopen(filename, 'r');
    if fid == -1
        error('Could not open file: %s', filename);
    end

    lines = textscan(fid, '%s', 'Delimiter', '\n');
    fclose(fid);

    % Convert cell array of strings to a matrix of integers
    num_rows = length(lines{1});
    num_cols = length(lines{1}{1});
    map = zeros(num_rows, num_cols, 'uint8');

    for i = 1:num_rows
        row_str = lines{1}{i};
        for j = 1:num_cols
            map(i, j) = str2double(row_str(j));
        end
    end
end

function rating = calculate_trailhead_rating(map, start_row, start_col)
    % Calculates the rating of a trailhead using Depth First Search (DFS).
    % The rating is the number of distinct hiking trails from the trailhead
    % to any cell with height 9.
    [rows, cols] = size(map);
    
    % Memoization table to store the number of paths from a cell to a '9'.
    % Initialized with -1 to indicate uncomputed values.
    memo = -ones(rows, cols, 'int64');

    rating = dfs_paths(map, start_row, start_col, memo);
end

function num_paths = dfs_paths(map, r, c, memo)
    % Recursive helper function for DFS to count paths.
    [rows, cols] = size(map);

    % Base cases:
    % 1. Out of bounds
    if r < 1 || r > rows || c < 1 || c > cols
        num_paths = 0;
        return;
    end

    % 2. Reached a '9'
    if map(r, c) == 9
        num_paths = 1;
        return;
    end

    % 3. Already computed (memoization)
    if memo(r, c) ~= -ones(1, 'int64')
        num_paths = memo(r, c);
        return;
    end

    % Recursive step: Explore valid neighbors
    current_height = map(r, c);
    num_paths = 0;

    % Define possible moves (up, down, left, right)
    dr = [-1, 1, 0, 0];
    dc = [0, 0, -1, 1];

    for i = 1:4
        next_r = r + dr(i);
        next_c = c + dc(i);

        % Check if the move is valid:
        % - Within bounds
        % - Height increases by exactly 1
        if next_r >= 1 && next_r <= rows && next_c >= 1 && next_c <= cols && ...
           map(next_r, next_c) == current_height + 1
            
            % Recursively call DFS for the valid neighbor
            num_paths = num_paths + dfs_paths(map, next_r, next_c, memo);
        end
    end

    % Store the computed result in the memoization table
    memo(r, c) = num_paths;
end

% Call the main function to start the program
main();
