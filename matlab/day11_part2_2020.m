
function main()
    % Read input from input.txt
    seats = readInput('input.txt');

    % Part 1: Simulate seating with adjacent visibility
    occupied_seats_part1 = simulateSeating(seats, 4, false);
    fprintf('Part 1: Total occupied seats: %d\n', occupied_seats_part1);

    % Part 2: Simulate seating with line-of-sight visibility
    occupied_seats_part2 = simulateSeating(seats, 5, true);
    fprintf('Part 2: Total occupied seats: %d\n', occupied_seats_part2);
end

function seats = readInput(filename)
    % Reads the seating layout from a file.
    % Returns a cell array where each cell contains a character representing a seat state.
    fid = fopen(filename, 'r');
    if fid == -1
        error('Could not open file: %s', filename);
    end
    fileContent = textscan(fid, '%s', 'Delimiter', '\n');
    fclose(fid);

    % Convert the cell array of strings to a cell array of characters
    seats = cellfun(@(s) s(:), fileContent{1}, 'UniformOutput', false);
end

function occupied_count = simulateSeating(initial_seats, tolerance, use_line_of_sight)
    % Simulates the seating process until equilibrium is reached.
    %
    % Args:
    %   initial_seats: A cell array of strings representing the initial seating layout.
    %   tolerance: The number of occupied adjacent/visible seats required to make an occupied seat empty.
    %   use_line_of_sight: A boolean indicating whether to use line-of-sight visibility (true) or adjacent visibility (false).
    %
    % Returns:
    %   The total number of occupied seats at equilibrium.

    current_seats = initial_seats;
    rows = length(current_seats);
    cols = length(current_seats{1});

    while true
        next_seats = cell(rows, 1);
        changed = false;

        for r = 1:rows
            next_row_chars = char(current_seats{r}); % Convert string to char array for modification
            for c = 1:cols
                current_state = current_seats{r}(c);

                if current_state == '.'
                    continue; % Floor never changes
                end

                occupied_neighbors = countOccupiedNeighbors(current_seats, r, c, tolerance, use_line_of_sight);

                if current_state == 'L' && occupied_neighbors == 0
                    next_row_chars(c) = '#'; % Empty seat becomes occupied
                    changed = true;
                elseif current_state == '#' && occupied_neighbors >= tolerance
                    next_row_chars(c) = 'L'; % Occupied seat becomes empty
                    changed = true;
                end
            end
            next_seats{r} = next_row_chars; % Convert char array back to string
        end

        if ~changed
            break; % Equilibrium reached
        end
        current_seats = next_seats;
    end

    % Count occupied seats
    occupied_count = 0;
    for r = 1:rows
        occupied_count = occupied_count + sum(current_seats{r} == '#');
    end
end

function occupied_count = countOccupiedNeighbors(seats, row, col, tolerance, use_line_of_sight)
    % Counts the number of occupied seats adjacent to or visible from a given seat.
    %
    % Args:
    %   seats: The current seating layout (cell array of strings).
    %   row: The row index of the seat.
    %   col: The column index of the seat.
    %   tolerance: The tolerance value (used for part 2).
    %   use_line_of_sight: Boolean indicating line-of-sight visibility.
    %
    % Returns:
    %   The count of occupied neighbors.

    rows = length(seats);
    cols = length(seats{1});
    occupied_count = 0;

    % Define the eight directions
    directions = [-1 -1; -1 0; -1 1; 0 -1; 0 1; 1 -1; 1 0; 1 1];

    for i = 1:size(directions, 1)
        dr = directions(i, 1);
        dc = directions(i, 2);

        if use_line_of_sight
            % Part 2: Line-of-sight visibility
            r = row + dr;
            c = col + dc;
            while r >= 1 && r <= rows && c >= 1 && c <= cols
                if seats{r}(c) == '#'
                    occupied_count = occupied_count + 1;
                    break; % Found an occupied seat in this direction
                elseif seats{r}(c) == 'L'
                    break; % Found an empty seat, stop looking in this direction
                end
                % Move to the next seat in this direction
                r = r + dr;
                c = c + dc;
            end
        else
            % Part 1: Adjacent visibility
            r = row + dr;
            c = col + dc;
            if r >= 1 && r <= rows && c >= 1 && c <= cols && seats{r}(c) == '#'
                occupied_count = occupied_count + 1;
            end
        end
    end
end

% Call the main function to execute the program
main();
