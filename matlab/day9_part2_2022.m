
function rope_bridge()
    % Reads input from input.txt and simulates rope movement.
    % Prints the number of unique positions visited by the tail to standard output.

    % --- Configuration ---
    NUM_KNOTS = 10; % For Part Two, change to 2 for Part One

    % --- Initialization ---
    % Initialize knot positions. Each knot is [row, col].
    % We use a 2D array where each column represents a knot.
    knots = zeros(2, NUM_KNOTS);

    % Set to store unique tail positions.
    % We'll use a string representation of the coordinates for easy storage.
    visited_tail_positions = containers.Map('KeyType', 'char', 'ValueType', 'logical');
    visited_tail_positions(sprintf('%d,%d', knots(1, end), knots(2, end))) = true;

    % --- File Reading ---
    try
        fid = fopen('input.txt', 'r');
        if fid == -1
            error('Could not open input.txt for reading.');
        end
        moves = textscan(fid, '%s %d', 'Delimiter', ' ');
        fclose(fid);
    catch ME
        fprintf(2, 'Error reading input file: %s\n', ME.message);
        return;
    end

    % --- Simulation ---
    directions = moves{1};
    steps = moves{2};

    for i = 1:length(directions)
        direction = directions{i};
        num_steps = steps(i);

        for step = 1:num_steps
            % Move the head
            switch direction
                case 'U'
                    knots(1, 1) = knots(1, 1) + 1; % Row increases upwards
                case 'D'
                    knots(1, 1) = knots(1, 1) - 1; % Row decreases downwards
                case 'L'
                    knots(2, 1) = knots(2, 1) - 1; % Column decreases leftwards
                case 'R'
                    knots(2, 1) = knots(2, 1) + 1; % Column increases rightwards
            end

            % Update subsequent knots
            for k = 2:NUM_KNOTS
                head_pos = knots(:, k-1);
                tail_pos = knots(:, k);

                % Calculate the difference in position
                row_diff = head_pos(1) - tail_pos(1);
                col_diff = head_pos(2) - tail_pos(2);

                % Check if the tail needs to move
                if abs(row_diff) > 1 || abs(col_diff) > 1
                    % Tail needs to move
                    if row_diff > 0
                        knots(1, k) = knots(1, k) + 1; % Move down (towards head)
                    elseif row_diff < 0
                        knots(1, k) = knots(1, k) - 1; % Move up (towards head)
                    end

                    if col_diff > 0
                        knots(2, k) = knots(2, k) + 1; % Move right (towards head)
                    elseif col_diff < 0
                        knots(2, k) = knots(2, k) - 1; % Move left (towards head)
                    end
                end
            end

            % Record the tail's position after each step
            tail_position_str = sprintf('%d,%d', knots(1, end), knots(2, end));
            visited_tail_positions(tail_position_str) = true;
        end
    end

    % --- Output ---
    fprintf('%d\n', length(visited_tail_positions));
end

% To run this program:
% 1. Save the code as `rope_bridge.m`.
% 2. Create a file named `input.txt` in the same directory with your puzzle input.
% 3. Open MATLAB, navigate to the directory where you saved the files.
% 4. Run the script by typing `rope_bridge` in the MATLAB command window and pressing Enter.
