
function main()
    fid = fopen('input.txt', 'r');
    if fid == -1
        error('Unable to open input.txt');
    end
    
    initialState = textscan(fid, '%s', 'Delimiter', '\n');
    fclose(fid);
    
    initialState = initialState{1};
    
    activeCubes = containers.Map('KeyType', 'char', 'ValueType', 'logical');
    
    for y = 1:length(initialState)
        line = initialState{y};
        for x = 1:length(line)
            if line(x) == '#'
                coordStr = sprintf('%d,%d,%d', x-1, y-1, 0);
                activeCubes(coordStr) = true;
            end
        end
    end
    
    for cycle = 1:6
        activeCubes = simulateCycle(activeCubes);
    end
    
    disp(activeCubes.Count);
end

function newActiveCubes = simulateCycle(activeCubes)
    neighborCounts = containers.Map('KeyType', 'char', 'ValueType', 'double');
    
    keys = activeCubes.keys;
    for i = 1:length(keys)
        coordStr = keys{i};
        coords = str2double(strsplit(coordStr, ','));
        x = coords(1);
        y = coords(2);
        z = coords(3);
        
        for dz = -1:1
            for dy = -1:1
                for dx = -1:1
                    if dz == 0 && dy == 0 && dx == 0
                        continue
                    end
                    neighborX = x + dx;
                    neighborY = y + dy;
                    neighborZ = z + dz;
                    neighborCoordStr = sprintf('%d,%d,%d', neighborX, neighborY, neighborZ);
                    
                    if isKey(neighborCounts, neighborCoordStr)
                        neighborCounts(neighborCoordStr) = neighborCounts(neighborCoordStr) + 1;
                    else
                        neighborCounts(neighborCoordStr) = 1;
                    end
                end
            end
        end
    end
    
    newActiveCubes = containers.Map('KeyType', 'char', 'ValueType', 'logical');
    
    neighborKeys = neighborCounts.keys;
    for i = 1:length(neighborKeys)
        coordStr = neighborKeys{i};
        count = neighborCounts(coordStr);
        
        isActive = false;
        if isKey(activeCubes, coordStr)
            isActive = activeCubes(coordStr);
        end
        
        if (count == 3) || (count == 2 && isActive)
            newActiveCubes(coordStr) = true;
        end
    end
end

main();
