
function main()
    grid = readInput('input.txt');
    result = dijkstra(grid);
    disp(result);
end

function grid = readInput(filePath)
    fid = fopen(filePath, 'r');
    lines = textscan(fid, '%s', 'Delimiter', '\n');
    fclose(fid);
    lines = lines{1};
    
    rows = length(lines);
    cols = length(lines{1});
    grid = zeros(rows, cols);
    
    for i = 1:rows
        for j = 1:cols
            grid(i, j) = str2double(lines{i}(j));
        end
    end
end

function minRisk = dijkstra(grid)
    [rows, cols] = size(grid);
    riskLevels = inf(rows, cols);
    riskLevels(1, 1) = 0;
    
    priorityQueue = struct('x', {}, 'y', {}, 'risk', {});
    priorityQueue(1).x = 1;
    priorityQueue(1).y = 1;
    priorityQueue(1).risk = 0;
    
    directions = [1, 0; 0, 1; -1, 0; 0, -1];
    
    while ~isempty(priorityQueue)
        [~, idx] = min([priorityQueue.risk]);
        current = priorityQueue(idx);
        priorityQueue(idx) = [];
        
        x = current.x;
        y = current.y;
        risk = current.risk;
        
        if x == rows && y == cols
            minRisk = risk;
            return;
        end
        
        for i = 1:size(directions, 1)
            newX = x + directions(i, 1);
            newY = y + directions(i, 2);
            
            if newX >= 1 && newY >= 1 && newX <= rows && newY <= cols
                newRisk = risk + grid(newX, newY);
                if newRisk < riskLevels(newX, newY)
                    riskLevels(newX, newY) = newRisk;
                    priorityQueue(end+1) = struct('x', newX, 'y', newY, 'risk', newRisk);
                end
            end
        end
    end
    minRisk = -1;
end

main();
