
function main()
    % Read input from input.txt
    try
        fid = fopen('input.txt', 'r');
        if fid == -1
            error('Could not open input.txt for reading.');
        end
        lines = textscan(fid, '%d,%d -> %d,%d', 'Delimiter', ' ');
        fclose(fid);
    catch ME
        fprintf(2, 'Error reading input file: %s\n', ME.message);
        return;
    end

    % Extract coordinates
    x1 = lines{1};
    y1 = lines{2};
    x2 = lines{3};
    y2 = lines{4};

    % Initialize a grid to store vent counts.
    % We need to determine the maximum coordinate to size the grid.
    maxCoord = max([x1; y1; x2; y2]);
    grid = zeros(maxCoord + 1, maxCoord + 1);

    % Process each line segment
    numSegments = length(x1);
    for i = 1:numSegments
        % Only consider horizontal and vertical lines
        if x1(i) == x2(i) || y1(i) == y2(i)
            % Determine the start and end points for iteration
            startX = min(x1(i), x2(i));
            endX = max(x1(i), x2(i));
            startY = min(y1(i), y2(i));
            endY = max(y1(i), y2(i));

            % Increment the grid for each point on the line
            for x = startX:endX
                for y = startY:endY
                    grid(y + 1, x + 1) = grid(y + 1, x + 1) + 1;
                end
            end
        end
    end

    % Count points where at least two lines overlap
    overlapCount = sum(grid(:) >= 2);

    % Print the output to standard output
    fprintf('Number of points where at least two lines overlap: %d\n', overlapCount);
end

% To run this program:
% 1. Save the code as a .m file (e.g., hydrothermal_venture.m).
% 2. Create a file named input.txt in the same directory and paste your input data into it.
% 3. Open MATLAB, navigate to the directory where you saved the file.
% 4. Run the program by typing 'main' in the MATLAB command window and pressing Enter.
