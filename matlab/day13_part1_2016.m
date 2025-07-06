
function main()
    favoriteNumber = readInput();
    start = [1, 1];
    target = [31, 39];
    steps = bfs(start, target, favoriteNumber);
    disp(steps);
end

function favoriteNumber = readInput()
    fid = fopen('input.txt', 'r');
    favoriteNumber = fscanf(fid, '%d');
    fclose(fid);
end

function isWall = checkWall(favoriteNumber, x, y)
    num = x*x + 3*x + 2*x*y + y + y*y + favoriteNumber;
    isWall = mod(sum(dec2bin(num) - '0'), 2) ~= 0;
end

function steps = bfs(start, target, favoriteNumber)
    visited = containers.Map('KeyType', 'char', 'ValueType', 'logical');
    queue = {start};
    steps = 0;
    
    while ~isempty(queue)
        levelSize = numel(queue);
        for i = 1:levelSize
            currentPoint = queue{1};
            queue(1) = [];
            
            if isequal(currentPoint, target)
                steps = steps;
                return;
            end
            
            deltas = {[1, 0], [-1, 0], [0, 1], [0, -1]};
            for j = 1:numel(deltas)
                delta = deltas{j};
                nextPoint = currentPoint + delta;
                
                if nextPoint(1) >= 0 && nextPoint(2) >= 0 && ~checkWall(favoriteNumber, nextPoint(1), nextPoint(2))
                    key = sprintf('%d,%d', nextPoint(1), nextPoint(2));
                    if ~isKey(visited, key)
                        visited(key) = true;
                        queue{end+1} = nextPoint;
                    end
                end
            end
        end
        steps = steps + 1;
    end
    steps = -1;
end

main();
