
function main()
    % Read the tree heights from input.txt
    tree_map = read_input('input.txt');

    % Calculate the number of visible trees
    visible_trees_count = count_visible_trees(tree_map);

    % Print the result to standard output
    fprintf('Number of visible trees: %d\n', visible_trees_count);
end

function tree_map = read_input(filename)
    % Reads the tree heights from a file and returns them as a numeric matrix.
    % Each line in the file is treated as a row of trees.
    fid = fopen(filename, 'r');
    if fid == -1
        error('Could not open file: %s', filename);
    end

    % Read all lines from the file
    lines = textscan(fid, '%s', 'Delimiter', '\n');
    fclose(fid);

    % Convert cell array of strings to a numeric matrix
    % Each character in each string is converted to its numeric value.
    num_rows = length(lines{1});
    num_cols = length(lines{1}{1});
    tree_map = zeros(num_rows, num_cols);

    for i = 1:num_rows
        row_str = lines{1}{i};
        for j = 1:num_cols
            tree_map(i, j) = str2double(row_str(j));
        end
    end
end

function visible_count = count_visible_trees(tree_map)
    % Counts the number of trees visible from outside the grid.
    % A tree is visible if all trees between it and an edge are shorter.

    [num_rows, num_cols] = size(tree_map);

    % Initialize a logical matrix to keep track of visible trees.
    % All trees on the edge are initially considered visible.
    is_visible = false(num_rows, num_cols);
    is_visible(1, :) = true;   % Top row
    is_visible(num_rows, :) = true; % Bottom row
    is_visible(:, 1) = true;   % Leftmost column
    is_visible(:, num_cols) = true; % Rightmost column

    % Check visibility from each of the four directions for interior trees.

    % From Left
    for r = 2:(num_rows - 1)
        max_height_so_far = tree_map(r, 1);
        for c = 2:(num_cols - 1)
            if tree_map(r, c) > max_height_so_far
                is_visible(r, c) = true;
                max_height_so_far = tree_map(r, c);
            end
        end
    end

    % From Right
    for r = 2:(num_rows - 1)
        max_height_so_far = tree_map(r, num_cols);
        for c = (num_cols - 1):-1:2
            if tree_map(r, c) > max_height_so_far
                is_visible(r, c) = true;
                max_height_so_far = tree_map(r, c);
            end
        end
    end

    % From Top
    for c = 2:(num_cols - 1)
        max_height_so_far = tree_map(1, c);
        for r = 2:(num_rows - 1)
            if tree_map(r, c) > max_height_so_far
                is_visible(r, c) = true;
                max_height_so_far = tree_map(r, c);
            end
        end
    end

    % From Bottom
    for c = 2:(num_cols - 1)
        max_height_so_far = tree_map(num_rows, c);
        for r = (num_rows - 1):-1:2
            if tree_map(r, c) > max_height_so_far
                is_visible(r, c) = true;
                max_height_so_far = tree_map(r, c);
            end
        end
    end

    % The total count is the sum of all true values in the is_visible matrix.
    visible_count = sum(is_visible(:));
end

% To run this program:
% 1. Save the code as a .m file (e.g., treetop_treehouse.m).
% 2. Create a file named 'input.txt' in the same directory with your tree data.
% 3. Open MATLAB, navigate to the directory where you saved the files.
% 4. Type 'treetop_treehouse' in the MATLAB command window and press Enter.
