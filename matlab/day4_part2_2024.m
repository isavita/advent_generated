
function solve()
% solve is the main entry point for the program.
% It reads a character grid from "input.txt", counts X-MAS patterns,
% and prints the result.

    filename = 'input.txt';
    fid = fopen(filename, 'r');
    if fid == -1
        error('Error opening file: %s', filename);
    end

    lines = {};
    tline = fgetl(fid);
    while ischar(tline)
        if ~isempty(tline) % Ignore empty lines
            lines{end+1} = tline;
        end
        tline = fgetl(fid);
    end
    fclose(fid);

    if isempty(lines)
        grid = char([]); % Empty grid
        rows = 0;
        cols = 0;
    else
        rows = length(lines);
        cols = length(lines{1}); % Assume all lines have the same length as the first
        
        % Pre-allocate character array for the grid
        grid = char(zeros(rows, cols));
        for i = 1:rows
            grid(i, :) = lines{i};
        end
    end

    count = countXMASPatterns(grid, rows, cols);
    fprintf('X-MAS patterns appear %d times in the word search\n', count);
end

function result = checkMAS(grid, rows, cols, r, c, dr, dc)
% checkMAS checks if a "MAS" or "SAM" pattern exists starting at (r,c)
% in the direction (dr,dc).
% grid: The character grid.
% rows, cols: Dimensions of the grid.
% r, c: 1-indexed starting row and column.
% dr, dc: Row and column increments for the direction.

    word = 'MAS';
    
    % Check for "MAS" (forward)
    forward = true;
    for i = 0:2
        nr = r + dr * i;
        nc = c + dc * i;
        % Check boundaries and character match
        if nr < 1 || nc < 1 || nr > rows || nc > cols || grid(nr, nc) ~= word(i+1)
            forward = false;
            break;
        end
    end

    % Check for "SAM" (backward)
    backward = true;
    for i = 0:2
        nr = r + dr * i;
        nc = c + dc * i;
        % Check boundaries and character match (word(3) is 'S', word(2) is 'A', word(1) is 'M')
        if nr < 1 || nc < 1 || nr > rows || nc > cols || grid(nr, nc) ~= word(3-i)
            backward = false;
            break;
        end
    end
    
    result = forward || backward;
end

function result = checkXMAS(grid, rows, cols, r, c)
% checkXMAS checks if an 'XMAS' pattern is centered at (r,c).
% An XMAS pattern requires the center 'A' to be part of two intersecting
% "MAS" or "SAM" patterns forming an 'X' shape.
% grid: The character grid.
% rows, cols: Dimensions of the grid.
% r, c: 1-indexed row and column of the center 'A'.

    % The C logic checks two pairs of intersecting MAS patterns:
    % Pair 1: (Top-Left to Bottom-Right) AND (Top-Right to Bottom-Left)
    % Pair 2: (Bottom-Left to Top-Right) AND (Bottom-Right to Top-Left)

    % Check the first pair of intersecting MAS patterns
    % MAS from (r-1, c-1) to (r+1, c+1)
    cond1_part1 = checkMAS(grid, rows, cols, r - 1, c - 1, 1, 1);
    % MAS from (r-1, c+1) to (r+1, c-1)
    cond1_part2 = checkMAS(grid, rows, cols, r - 1, c + 1, 1, -1);
    cond1 = cond1_part1 && cond1_part2;

    % Check the second pair of intersecting MAS patterns
    % MAS from (r+1, c-1) to (r-1, c+1)
    cond2_part1 = checkMAS(grid, rows, cols, r + 1, c - 1, -1, 1);
    % MAS from (r+1, c+1) to (r-1, c-1)
    cond2_part2 = checkMAS(grid, rows, cols, r + 1, c + 1, -1, -1);
    cond2 = cond2_part1 && cond2_part2;
    
    result = cond1 || cond2;
end

function count = countXMASPatterns(grid, rows, cols)
% countXMASPatterns iterates through the grid to find and count X-MAS patterns.
% grid: The character grid.
% rows, cols: Dimensions of the grid.

    % An X-MAS pattern requires at least a 3x3 area.
    if rows < 3 || cols < 3
        count = 0;
        return;
    end
    
    count = 0;
    % Iterate through the grid. The center 'A' of an X-MAS pattern
    % cannot be in the first or last row/column, as it needs surrounding characters.
    for r = 2 : rows - 1 % MATLAB is 1-indexed, so r from 2 to rows-1
        for c = 2 : cols - 1 % c from 2 to cols-1
            if grid(r, c) == 'A'
                if checkXMAS(grid, rows, cols, r, c)
                    count = count + 1;
                end
            end
        end
    end
end
