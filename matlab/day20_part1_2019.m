
function solve()
% solve is the main entry point for the program.
% It reads the maze from "input.txt", finds the shortest path, and prints the result.

    % MAX_SIZE is used for consistent linear indexing in the teleport_map,
    % matching the C code's fixed array size.
    MAX_SIZE = 200; 

    % Parse the input file to get the grid, start/end points, and teleport map.
    [grid, aa, zz, teleport_map, xMax, yMax] = parse(MAX_SIZE);
    
    % Perform Breadth-First Search to find the shortest path.
    result = BFS(grid, aa, zz, teleport_map, xMax, yMax, MAX_SIZE);
    
    % Print the result.
    fprintf('%d\n', result);
end

function [grid, aa, zz, teleport_map, xMax, yMax] = parse(MAX_SIZE)
% parse reads the maze layout from "input.txt" and processes portal information.
% It returns:
%   grid: A character matrix representing the maze.
%   aa: The 0-indexed [x, y] coordinates of the 'AA' portal entry point.
%   zz: The 0-indexed [x, y] coordinates of the 'ZZ' portal entry point.
%   teleport_map: A (MAX_SIZE*MAX_SIZE)x2 matrix mapping 0-indexed portal entry points
%                 (via their 1-based linear index) to their corresponding 0-indexed exit points.
%   xMax, yMax: Dimensions of the grid.

    % Open the input file.
    fid = fopen('input.txt', 'r');
    if fid == -1
        error('Error opening file input.txt');
    end
    % Read all lines from the file.
    lines = textscan(fid, '%s', 'Delimiter', '\n', 'Whitespace', '');
    fclose(fid);
    lines = lines{1}; % Extract cell array of strings

    % Determine grid dimensions.
    xMax = numel(lines);
    yMax = 0;
    for i = 1:xMax
        yMax = max(yMax, numel(lines{i}));
    end

    % Initialize the grid with spaces and populate it.
    grid = repmat(' ', xMax, yMax);
    for i = 1:xMax
        line = lines{i};
        grid(i, 1:numel(line)) = line;
    end

    % Initialize the teleport map. Each entry stores a 0-indexed [tx, ty] point.
    % The map is indexed by the 1-based linear index of the source 0-indexed point.
    teleport_map = zeros(MAX_SIZE*MAX_SIZE, 2); 
    aa = []; % Initialize AA portal point
    zz = []; % Initialize ZZ portal point

    % Cache for linking portal pairs: stores portalName (string) and portalPoint (0-indexed [x,y])
    cache_names = cell(0,1);
    cache_points = zeros(0,2);

    % Iterate through the grid to find and process portals.
    % Coordinates (x, y) are 0-indexed internally for consistency with C code.
    for x = 0:(xMax-1) 
        for y = 0:(yMax-1) 
            % Get the character at the current 0-indexed (x,y) position (1-indexed for grid access).
            curr_char = grid(x+1, y+1); 

            % Skip if the current character is not a letter.
            if ~isletter(curr_char)
                continue;
            end

            portalName = '';
            portalPoint = []; % The '.' point associated with the portal

            % Check for a vertical portal (current char is the top letter).
            % Ensure bounds are checked before accessing grid elements.
            if (x + 1 < xMax) && isletter(grid(x+1+1, y+1)) % Check char below
                c1 = curr_char;
                c2 = grid(x+1+1, y+1);
                portalName = [c1, c2];
                
                % Find the '.' point adjacent to this portal pair.
                % Check below the second letter (x+1,y) -> (x+2,y)
                if (x + 2 + 1 <= xMax) && (grid(x+2+1, y+1) == '.') 
                    portalPoint = [x+2, y]; % 0-indexed point
                % Check above the first letter (x,y) -> (x-1,y)
                elseif (x - 1 >= 0) && (grid(x-1+1, y+1) == '.') 
                    portalPoint = [x-1, y]; % 0-indexed point
                end
            end

            % If not a vertical portal, check for a horizontal portal (current char is the left letter).
            % Ensure bounds are checked before accessing grid elements.
            if isempty(portalPoint) && (y + 1 < yMax) && isletter(grid(x+1, y+1+1)) % Check char right
                c1 = curr_char;
                c2 = grid(x+1, y+1+1);
                portalName = [c1, c2];

                % Find the '.' point adjacent to this portal pair.
                % Check right of the second letter (x,y+1) -> (x,y+2)
                if (y + 2 + 1 <= yMax) && (grid(x+1, y+2+1) == '.') 
                    portalPoint = [x, y+2]; % 0-indexed point
                % Check left of the first letter (x,y) -> (x,y-1)
                elseif (y - 1 >= 0) && (grid(x+1, y-1+1) == '.') 
                    portalPoint = [x, y-1]; % 0-indexed point
                end
            end
            
            % If no valid portal point was found, continue to the next character.
            if isempty(portalPoint)
                continue; 
            end

            % Handle the special 'AA' and 'ZZ' portals.
            if strcmp(portalName, 'AA')
                aa = portalPoint;
                continue; % 'AA' is a start point, not a teleport pair
            end
            if strcmp(portalName, 'ZZ')
                zz = portalPoint;
                continue; % 'ZZ' is an end point, not a teleport pair
            end

            % For other portals, link them using the cache.
            found_in_cache = false;
            for k = 1:numel(cache_names)
                % If a matching portal name is found in the cache.
                if strcmp(cache_names{k}, portalName)
                    p1 = cache_points(k,:); % The first point found for this portal name
                    p2 = portalPoint;       % The second point found for this portal name
                    
                    % Store the teleport mapping in both directions.
                    % Linear index for teleport_map is 1-based, derived from 0-indexed point.
                    teleport_map(sub2ind([MAX_SIZE, MAX_SIZE], p1(1)+1, p1(2)+1), :) = p2;
                    teleport_map(sub2ind([MAX_SIZE, MAX_SIZE], p2(1)+1, p2(2)+1), :) = p1;
                    
                    % Mark this entry in the cache as used (by clearing its name)
                    % so it's not matched again.
                    cache_names{k} = ''; 
                    found_in_cache = true;
                    break;
                end
            end

            % If the portal name was not found in the cache, add it.
            if ~found_in_cache
                cache_names{end+1} = portalName;
                cache_points(end+1,:) = portalPoint;
            end
        end
    end
end

function p_next = getNeighbours(p_curr, dir)
% getNeighbours calculates the coordinates of a neighboring point.
% p_curr: The current 0-indexed [x, y] point.
% dir: Direction (0: y+1, 1: x+1, 2: y-1, 3: x-1, matching C code's directions).
% p_next: The 0-indexed [x, y] coordinates of the neighbor.

    p_next = p_curr; % Start with current point
    switch dir
        case 0 % C's dir 0: y+1 (down in grid)
            p_next(2) = p_next(2) + 1;
        case 1 % C's dir 1: x+1 (right in grid)
            p_next(1) = p_next(1) + 1;
        case 2 % C's dir 2: y-1 (up in grid)
            p_next(2) = p_next(2) - 1;
        case 3 % C's dir 3: x-1 (left in grid)
            p_next(1) = p_next(1) - 1;
    end
end

function result = BFS(grid, aa, zz, teleport_map, xMax, yMax, MAX_SIZE)
% BFS performs a Breadth-First Search to find the shortest path from 'AA' to 'ZZ'.
% grid: The maze grid.
% aa: The 0-indexed [x, y] start point.
% zz: The 0-indexed [x, y] end point.
% teleport_map: The map of portal connections.
% xMax, yMax: Dimensions of the grid.
% MAX_SIZE: Constant used for linear indexing.
% result: The shortest path length, or -1 if no path is found.

    % discovered: A logical array to keep track of visited points (1-indexed).
    discovered = false(xMax, yMax); 
    % queue: Stores 0-indexed [x, y] points to visit. Pre-allocate for efficiency.
    queue = zeros(MAX_SIZE*MAX_SIZE, 2); 
    head = 1; % Queue head pointer
    tail = 1; % Queue tail pointer

    % Start BFS from the 'AA' portal entry point.
    discovered(aa(1)+1, aa(2)+1) = true; % Mark AA as discovered (1-indexed)
    queue(tail,:) = aa; % Add AA to the queue (0-indexed)
    tail = tail + 1;
    
    depth = 0; % Current path length (number of steps)

    % Main BFS loop.
    while head < tail
        levelSize = tail - head; % Number of nodes at the current depth level.
        for k = 1:levelSize
            curr = queue(head,:); % Get the current point (0-indexed) from the queue.
            head = head + 1;

            % Check if the target 'ZZ' is reached.
            if curr(1) == zz(1) && curr(2) == zz(2)
                result = depth;
                return;
            end

            % Explore the 4 direct neighbours (up, right, down, left).
            for i = 0:3 % Iterate through C's directions (0, 1, 2, 3)
                next = getNeighbours(curr, i); % Calculate neighbour point (0-indexed).

                % Check if the neighbour is within grid bounds.
                if next(1) < 0 || next(1) >= xMax || next(2) < 0 || next(2) >= yMax
                    continue;
                end
                
                % Convert 0-indexed neighbour point to 1-indexed for grid/discovered array access.
                next_x_1idx = next(1) + 1;
                next_y_1idx = next(2) + 1;

                dest_char = grid(next_x_1idx, next_y_1idx);

                switch dest_char
                    case '#' % Wall: cannot move here.
                        continue; 
                    case '.' % Open path: can move here.
                        if ~discovered(next_x_1idx, next_y_1idx)
                            discovered(next_x_1idx, next_y_1idx) = true;
                            queue(tail,:) = next; % Add to queue (0-indexed).
                            tail = tail + 1;
                        end
                    otherwise % Must be a letter: implies 'curr' is a portal entry point.
                        % The C code's logic implies that if 'dest' is a letter,
                        % it means 'curr' was the '.' point adjacent to the portal letters.
                        % So, we look up the teleport destination from 'curr'.
                        
                        % Get the linear index for 'curr' (0-indexed point to 1-indexed map).
                        teleport_idx = sub2ind([MAX_SIZE, MAX_SIZE], curr(1)+1, curr(2)+1);
                        teleport_point = teleport_map(teleport_idx, :); % This is 0-indexed.

                        % Check if a valid teleport point exists (not [0,0] which is default for zeros(...,2)).
                        if any(teleport_point ~= 0) 
                            % Convert 0-indexed teleport point to 1-indexed for discovered array.
                            teleport_x_1idx = teleport_point(1) + 1;
                            teleport_y_1idx = teleport_point(2) + 1;

                            if ~discovered(teleport_x_1idx, teleport_y_1idx)
                                discovered(teleport_x_1idx, teleport_y_1idx) = true;
                                queue(tail,:) = teleport_point; % Add to queue (0-indexed).
                                tail = tail + 1;
                            end
                        end
                end
            end
        end
        depth = depth + 1; % Increment depth after processing all nodes at the current level.
    end
    
    result = -1; % If the loop finishes, no path to ZZ was found.
end
