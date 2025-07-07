
function main()
    % Read nanobot data from input.txt
    nanobots = readNanobots('input.txt');

    % Find the nanobot with the largest signal radius
    [~, strongestIdx] = max([nanobots.r]);
    strongestNanobot = nanobots(strongestIdx);

    % Count nanobots in range of the strongest nanobot
    inRangeCount = countInRange(strongestNanobot, nanobots);

    % Print the result to standard output
    fprintf('Number of nanobots in range of the strongest nanobot: %d\n', inRangeCount);
end

function nanobots = readNanobots(filename)
    % Reads nanobot data from a file.
    % Each line is expected to be in the format: pos=<x,y,z>, r=<radius>
    
    fid = fopen(filename, 'r');
    if fid == -1
        error('Could not open file: %s', filename);
    end

    % Pre-allocate for efficiency
    nanobots = struct('x', {}, 'y', {}, 'z', {}, 'r', {});
    
    line = fgetl(fid);
    while ischar(line)
        % Use regular expressions to extract position and radius
        match = regexp(line, 'pos=<(-?\d+),(-?\d+),(-?\d+)>, r=(-?\d+)', 'tokens');
        
        if ~isempty(match)
            tokens = match{1};
            nanobot.x = str2double(tokens{1});
            nanobot.y = str2double(tokens{2});
            nanobot.z = str2double(tokens{3});
            nanobot.r = str2double(tokens{4});
            nanobots = [nanobots, nanobot]; % Append to the struct array
        end
        
        line = fgetl(fid);
    end
    
    fclose(fid);
end

function count = countInRange(strongest, allNanobots)
    % Counts how many nanobots are within Manhattan distance of the strongest nanobot.
    
    count = 0;
    for i = 1:length(allNanobots)
        % Calculate Manhattan distance
        distance = abs(strongest.x - allNanobots(i).x) + ...
                   abs(strongest.y - allNanobots(i).y) + ...
                   abs(strongest.z - allNanobots(i).z);
        
        % Check if the nanobot is in range
        if distance <= strongest.r
            count = count + 1;
        end
    end
end

% To run this program, save it as a .m file (e.g., solve_day23.m)
% and then execute it from the MATLAB command window by typing:
% solve_day23()
