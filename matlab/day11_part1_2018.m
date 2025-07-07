
function main()
    % Read the grid serial number from input.txt
    serialNumber = readSerialNumber('input.txt');

    % Define grid dimensions
    gridSize = 300;

    % Initialize the power grid
    powerGrid = zeros(gridSize, gridSize);

    % Calculate power level for each fuel cell
    for x = 1:gridSize
        for y = 1:gridSize
            powerGrid(x, y) = calculatePowerLevel(x, y, serialNumber);
        end
    end

    % Find the 3x3 square with the largest total power
    [maxPower, topLeftX, topLeftY] = findMaxPowerSquare(powerGrid, 3);

    % Print the result to standard output
    fprintf('The top-left coordinate of the 3x3 square with the largest total power is: %d,%d\n', topLeftX, topLeftY);
end

function serialNumber = readSerialNumber(filename)
    % Reads the serial number from the first line of the specified file.
    try
        fid = fopen(filename, 'r');
        if fid == -1
            error('Could not open file: %s', filename);
        end
        serialNumber = fscanf(fid, '%d', 1);
        fclose(fid);
    catch ME
        fprintf(2, 'Error reading serial number: %s\n', ME.message);
        rethrow(ME); % Re-throw the exception to halt execution
    end
end

function powerLevel = calculatePowerLevel(x, y, serialNumber)
    % Calculates the power level for a single fuel cell.
    rackID = x + 10;
    powerLevel = (rackID * y + serialNumber) * rackID;
    % Keep only the hundreds digit
    powerLevel = floor(mod(powerLevel, 1000) / 100);
    % Subtract 5
    powerLevel = powerLevel - 5;
end

function [maxPower, topLeftX, topLeftY] = findMaxPowerSquare(powerGrid, squareSize)
    % Finds the square of a given size with the maximum total power.
    gridSize = size(powerGrid, 1);
    maxPower = -inf; % Initialize with negative infinity
    topLeftX = -1;
    topLeftY = -1;

    % Iterate through all possible top-left corners of the square
    for x = 1:(gridSize - squareSize + 1)
        for y = 1:(gridSize - squareSize + 1)
            % Calculate the sum of power for the current square
            currentSquarePower = sum(sum(powerGrid(x:(x + squareSize - 1), y:(y + squareSize - 1))));

            % Update maxPower and coordinates if current square is better
            if currentSquarePower > maxPower
                maxPower = currentSquarePower;
                topLeftX = x;
                topLeftY = y;
            end
        end
    end
end

% --- Entry point of the program ---
% This ensures the main function is called when the script is executed.
if nargin == 0
    main();
end
