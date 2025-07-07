
function main()
    % Read input from input.txt
    try
        fileId = fopen('input.txt', 'r');
        if fileId == -1
            error('Could not open input.txt for reading.');
        end
        grid = textscan(fileId, '%s', 'Delimiter', '\n');
        fclose(fileId);
    catch ME
        fprintf(2, 'Error reading input file: %s\n', ME.message);
        return;
    end

    % Convert cell array of strings to a character array
    grid = char(grid{1});

    % Define constants
    NUM_MINUTES = 10;
    [rows, cols] = size(grid);

    % Simulate the landscape changes
    for minute = 1:NUM_MINUTES
        newGrid = grid; % Initialize new grid with current state

        for r = 1:rows
            for c = 1:cols
                % Count adjacent acres
                adjacentCounts = countAdjacent(grid, r, c);

                % Apply transformation rules
                switch grid(r, c)
                    case '.' % Open ground
                        if adjacentCounts.trees >= 3
                            newGrid(r, c) = '|'; % Becomes trees
                        end
                    case '|' % Trees
                        if adjacentCounts.lumberyards >= 3
                            newGrid(r, c) = '#'; % Becomes lumberyard
                        end
                    case '#' % Lumberyard
                        if adjacentCounts.lumberyards >= 1 && adjacentCounts.trees >= 1
                            % Remains lumberyard
                        else
                            newGrid(r, c) = '.'; % Becomes open ground
                        end
                end
            end
        end
        grid = newGrid; % Update the grid for the next minute
    end

    % Calculate the total resource value
    numTrees = sum(grid(:) == '|');
    numLumberyards = sum(grid(:) == '#');
    totalResourceValue = numTrees * numLumberyards;

    % Print the output to standard output
    fprintf('Total resource value after %d minutes: %d\n', NUM_MINUTES, totalResourceValue);
end

function counts = countAdjacent(grid, r, c)
    % Helper function to count adjacent acres of different types
    [rows, cols] = size(grid);
    counts.open = 0;
    counts.trees = 0;
    counts.lumberyards = 0;

    % Define relative positions of adjacent cells
    dr = [-1, -1, -1, 0, 0, 1, 1, 1];
    dc = [-1, 0, 1, -1, 1, -1, 0, 1];

    for i = 1:8
        nr = r + dr(i);
        nc = c + dc(i);

        % Check if the adjacent cell is within the grid boundaries
        if nr >= 1 && nr <= rows && nc >= 1 && nc <= cols
            switch grid(nr, nc)
                case '.'
                    counts.open = counts.open + 1;
                case '|'
                    counts.trees = counts.trees + 1;
                case '#'
                    counts.lumberyards = counts.lumberyards + 1;
            end
        end
    end
end

% Call the main function to execute the program
main();
