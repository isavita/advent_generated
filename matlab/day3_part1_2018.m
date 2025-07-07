
function main()
    % Read input from input.txt
    claims = readClaims('input.txt');

    % Calculate the total area with overlaps
    overlapArea = calculateOverlapArea(claims);

    % Print the output to standard output
    fprintf('%d\n', overlapArea);
end

function claims = readClaims(filename)
    % Reads claims from the specified file.
    % Each claim is parsed into a struct with fields: id, left, top, width, height.
    
    claims = struct('id', {}, 'left', {}, 'top', {}, 'width', {}, 'height', {});
    
    try
        fid = fopen(filename, 'r');
        if fid == -1
            error('Could not open file: %s', filename);
        end
        
        line = fgetl(fid);
        while ischar(line)
            % Parse the claim string using regular expressions
            % Example: #123 @ 3,2: 5x4
            tokens = regexp(line, '#(\d+) @ (\d+),(\d+): (\d+)x(\d+)', 'tokens');
            
            if ~isempty(tokens)
                token = tokens{1};
                claim.id = str2double(token{1});
                claim.left = str2double(token{2});
                claim.top = str2double(token{3});
                claim.width = str2double(token{4});
                claim.height = str2double(token{5});
                claims = [claims, claim]; %#ok<AGROW>
            end
            
            line = fgetl(fid);
        end
        fclose(fid);
    catch ME
        if fid ~= -1
            fclose(fid);
        end
        rethrow(ME);
    end
end

function overlapArea = calculateOverlapArea(claims)
    % Calculates the total number of square inches of fabric that are within two or more claims.
    % This is done by simulating the fabric grid and marking overlaps.

    % Determine the maximum dimensions of the fabric needed.
    % We can assume a large enough grid, but it's more efficient to size it based on input.
    max_x = 0;
    max_y = 0;
    for i = 1:length(claims)
        max_x = max(max_x, claims(i).left + claims(i).width);
        max_y = max(max_y, claims(i).top + claims(i).height);
    end
    
    % Initialize a grid to represent the fabric.
    % Each cell will store the number of claims that cover it.
    % Using a sparse matrix can be more memory-efficient if the fabric is very large
    % and claims are sparse, but for this problem, a dense matrix is likely fine
    % and simpler to implement.
    fabricGrid = zeros(max_y, max_x, 'int8'); % Use int8 for memory efficiency

    % Iterate through each claim and mark the fabric grid.
    for i = 1:length(claims)
        claim = claims(i);
        % Increment the count for each square inch covered by the current claim.
        % Note: MATLAB uses 1-based indexing.
        for y = claim.top + 1 : claim.top + claim.height
            for x = claim.left + 1 : claim.left + claim.width
                fabricGrid(y, x) = fabricGrid(y, x) + 1;
            end
        end
    end
    
    % Count the number of cells that have a value of 2 or more.
    overlapArea = sum(fabricGrid(:) >= 2);
end

% To run this program, save it as a .m file (e.g., solve_day3.m)
% and then call the main function from the MATLAB command window:
% solve_day3()
% Ensure you have an input.txt file in the same directory with the claims.

% Example of how to call main if this script is the entry point:
if isempty(ver('matlab')) % Check if running in MATLAB environment
    % This block will not execute if run directly in MATLAB.
    % It's a common pattern for standalone scripts that might be executed
    % in other environments or for testing.
else
    % If running in MATLAB, you can call main directly.
    % For this specific challenge, we'll assume the user calls main.
    % If this were a script meant to be executed directly, you might add:
    % main();
end
