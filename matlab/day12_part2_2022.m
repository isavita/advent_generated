
function main()
    % Read input from input.txt
    heightmap = readHeightmap('input.txt');

    % Find start and end positions
    [startRow, startCol] = findPosition(heightmap, 'S');
    [endRow, endCol] = findPosition(heightmap, 'E');

    % Convert 'S' and 'E' to 'a' and 'z' for elevation calculations
    heightmap(startRow, startCol) = 'a';
    heightmap(endRow, endCol) = 'z';

    % --- Part One: Shortest path from 'S' to 'E' ---
    stepsPart1 = findShortestPath(heightmap, [startRow, startCol], [endRow, endCol]);
    fprintf('Part 1: Fewest steps from S to E: %d\n', stepsPart1);

    % --- Part Two: Shortest path from any 'a' to 'E' ---
    % Find all starting positions with elevation 'a'
    [aRows, aCols] = findPositions(heightmap, 'a');
    startPositionsPart2 = [aRows, aCols];

    % Initialize minimum steps to a large value
    minStepsPart2 = inf;

    % Iterate through all 'a' positions and find the shortest path to 'E'
    for i = 1:size(startPositionsPart2, 1)
        currentStart = startPositionsPart2(i, :);
        steps = findShortestPath(heightmap, currentStart, [endRow, endCol]);
        if steps < minStepsPart2
            minStepsPart2 = steps;
        end
    end
    fprintf('Part 2: Fewest steps from any "a" to E: %d\n', minStepsPart2);
end

function heightmap = readHeightmap(filename)
    % Reads the heightmap from a file.
    % Returns a character array representing the heightmap.
    fid = fopen(filename, 'r');
    if fid == -1
        error('Could not open file: %s', filename);
    end
    heightmap = textscan(fid, '%s', 'Delimiter', '\n');
    fclose(fid);
    heightmap = char(heightmap{1});
end

function [row, col] = findPosition(heightmap, targetChar)
    % Finds the row and column of the first occurrence of a target character.
    [rows, cols] = find(heightmap == targetChar);
    if isempty(rows)
        error('Target character "%c" not found in heightmap.', targetChar);
    end
    row = rows(1);
    col = cols(1);
end

function [rows, cols] = findPositions(heightmap, targetChar)
    % Finds all row and column indices of a target character.
    [rows, cols] = find(heightmap == targetChar);
end

function steps = findShortestPath(heightmap, startPos, endPos)
    % Implements a Breadth-First Search (BFS) to find the shortest path.
    % heightmap: Character array representing the elevation grid.
    % startPos: [row, col] of the starting position.
    % endPos: [row, col] of the ending position.
    % Returns the minimum number of steps.

    [numRows, numCols] = size(heightmap);
    
    % Initialize distance matrix with infinity
    distance = inf(numRows, numCols);
    
    % Queue for BFS, storing [row, col, dist]
    queue = zeros(numRows * numCols, 3);
    queueHead = 1;
    queueTail = 0;

    % Set start position distance and add to queue
    startRow = startPos(1);
    startCol = startPos(2);
    distance(startRow, startCol) = 0;
    queueTail = queueTail + 1;
    queue(queueTail, :) = [startRow, startCol, 0];

    % Possible moves: up, down, left, right
    dr = [-1, 1, 0, 0];
    dc = [0, 0, -1, 1];

    while queueHead <= queueTail
        % Dequeue the current position
        currentRow = queue(queueHead, 1);
        currentCol = queue(queueHead, 2);
        currentDist = queue(queueHead, 3);
        queueHead = queueHead + 1;

        % If we reached the end position, return the distance
        if currentRow == endPos(1) && currentCol == endPos(2)
            steps = currentDist;
            return;
        end

        % Explore neighbors
        for i = 1:4
            nextRow = currentRow + dr(i);
            nextCol = currentCol + dc(i);

            % Check if the neighbor is within bounds
            if nextRow >= 1 && nextRow <= numRows && nextCol >= 1 && nextCol <= numCols
                
                % Get current and next elevations
                currentElevation = heightmap(currentRow, currentCol);
                nextElevation = heightmap(nextRow, nextCol);

                % Check if the move is valid (elevation difference <= 1)
                if nextElevation <= currentElevation + 1
                    % If this path is shorter than the previously recorded one
                    if currentDist + 1 < distance(nextRow, nextCol)
                        distance(nextRow, nextCol) = currentDist + 1;
                        queueTail = queueTail + 1;
                        queue(queueTail, :) = [nextRow, nextCol, currentDist + 1];
                    end
                end
            end
        end
    end
    
    % If the end position is not reachable
    steps = inf; 
end

% Call the main function to execute the program
main();
