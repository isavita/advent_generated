
function main()

% Read input from "input.txt"
fid = fopen('input.txt', 'r');
if fid == -1
    error('Error: Could not open input.txt');
end

% Read all lines into a cell array of strings
lines = textscan(fid, '%s', 'Delimiter', '\n', 'Whitespace', '');
grid = lines{1}; % grid is a cell array of strings
fclose(fid);

rows = numel(grid); % Number of rows in the grid

% Initialize position and direction
x = 0; % Column index (1-based)
y = 1; % Row index (1-based), starting at the first row
dx = 0; % Change in x (column)
dy = 1; % Change in y (row), initial direction is down (positive y)
steps = 0;

% Find the starting '|' in the first row
first_row_str = grid{1};
start_col_indices = strfind(first_row_str, '|');
if isempty(start_col_indices)
    error('Error: No starting point "|" found in the first row.');
end
x = start_col_indices(1); % Take the first occurrence

% Simulation loop
while true
    % Check if current position is out of bounds or points to a non-existent character
    if y < 1 || y > rows || x < 1 || x > length(grid{y})
        break; % Out of bounds
    end
    
    current_char = grid{y}(x);
    
    % Check for ' ' (space) which signifies the end of the path
    if current_char == ' '
        break;
    end
    
    steps = steps + 1; % Increment step count for the current cell
    
    % Handle turns at '+' intersections
    if current_char == '+'
        if dx == 0 % Was moving vertically (dy is -1 or 1)
            % Try to turn left (x-1)
            % Check if x-1 is a valid column index for the current row
            % and if the character at grid{y}(x-1) is '-' or a letter
            can_turn_left = (x - 1 >= 1) && (x - 1 <= length(grid{y})) && ...
                            (grid{y}(x-1) == '-' || (grid{y}(x-1) >= 'A' && grid{y}(x-1) <= 'Z'));
            
            if can_turn_left
                dx = -1; % Turn left
            else
                dx = 1; % Otherwise, turn right (as per C code's logic)
            end
            dy = 0; % Stop vertical movement
        else % Was moving horizontally (dx is -1 or 1)
            % Try to turn up (y-1)
            % Check if y-1 is a valid row index
            % and if x is a valid column index for the row above (grid{y-1})
            % and if the character at grid{y-1}(x) is '|' or a letter
            can_turn_up = (y - 1 >= 1) && (y - 1 <= rows) && ...
                          (x >= 1) && (x <= length(grid{y-1})) && ...
                          (grid{y-1}(x) == '|' || (grid{y-1}(x) >= 'A' && grid{y-1}(x) <= 'Z'));
            
            if can_turn_up
                dy = -1; % Turn up
            else
                dy = 1; % Otherwise, turn down (as per C code's logic)
            end
            dx = 0; % Stop horizontal movement
        end
    end
    
    % Move to the next position
    x = x + dx;
    y = y + dy;
end

% Print the total number of steps
fprintf('%d\n', steps);

end
