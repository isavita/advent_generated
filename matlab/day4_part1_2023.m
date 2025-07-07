
function totalPoints = scratchcard_solver()
% SCRATCHCARD_SOLVER Reads scratchcard data from input.txt and calculates total points.
%
% This function reads each line from the 'input.txt' file, parses the
% winning numbers and the player's numbers for each scratchcard, and
% calculates the points for each card. The total points from all cards
% are then printed to the standard output.
%
% The point calculation is as follows:
% - The first matching number is worth 1 point.
% - Each subsequent matching number doubles the point value.
%
% Example:
% Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
% Winning numbers: {41, 48, 83, 86, 17}
% Player's numbers: {83, 86, 6, 31, 17, 9, 48, 53}
% Matches: 48, 83, 17, 86 (4 matches)
% Points: 1 (for 48) + 2 (for 83) + 4 (for 17) + 8 (for 86) = 15 points.
%
% The function assumes the input file 'input.txt' is in the same directory
% as the script.

    totalPoints = 0;

    try
        % Open the input file for reading.
        fileID = fopen('input.txt', 'r');
        if fileID == -1
            error('Could not open input.txt for reading.');
        end

        % Read the file line by line.
        line = fgetl(fileID);
        while ischar(line)
            % Process each line (scratchcard).
            cardPoints = processScratchcard(line);
            totalPoints = totalPoints + cardPoints;

            % Read the next line.
            line = fgetl(fileID);
        end

        % Close the file.
        fclose(fileID);

        % Print the total points to standard output.
        fprintf('Total points: %d\n', totalPoints);

    catch ME
        % Handle any errors that occur during file processing.
        fprintf('Error: %s\n', ME.message);
        if exist('fileID', 'var') && fileID ~= -1
            fclose(fileID); % Ensure file is closed even if an error occurs.
        end
    end
end

function points = processScratchcard(cardLine)
% PROCESSCRATCHCARD Parses a single scratchcard line and calculates its points.
%
% Args:
%   cardLine: A string representing a single scratchcard line from the input.
%
% Returns:
%   points: The calculated points for the given scratchcard.

    % Remove the "Card X: " prefix.
    cardData = strsplit(cardLine, ':');
    numbersPart = cardData{2};

    % Split the numbers into winning numbers and player's numbers.
    numberLists = strsplit(numbersPart, '|');
    winningNumbersStr = strtrim(numberLists{1});
    playerNumbersStr = strtrim(numberLists{2});

    % Convert number strings to numeric arrays.
    % Use textscan for robust parsing of multiple spaces.
    winningNumbers = textscan(winningNumbersStr, '%d');
    playerNumbers = textscan(playerNumbersStr, '%d');

    % Extract the numeric arrays from the cell arrays returned by textscan.
    winningNumbers = winningNumbers{1};
    playerNumbers = playerNumbers{1};

    % Find the common numbers between winning and player numbers.
    % Using ismember is efficient for this.
    commonNumbers = ismember(playerNumbers, winningNumbers);

    % Count the number of matches.
    numMatches = sum(commonNumbers);

    % Calculate points based on the number of matches.
    if numMatches > 0
        points = 2^(numMatches - 1);
    else
        points = 0;
    end
end

% To run this program:
% 1. Save the code as 'scratchcard_solver.m'.
% 2. Create a file named 'input.txt' in the same directory.
% 3. Populate 'input.txt' with your scratchcard data, one card per line.
% 4. In the MATLAB command window, type:
%    scratchcard_solver()
