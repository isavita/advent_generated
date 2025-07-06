
function main()
    fid = fopen('input.txt', 'r');
    if fid == -1
        error('Error opening file: input.txt');
    end
    
    nodes = containers.Map('KeyType', 'char', 'ValueType', 'uint64');
    parentMap = containers.Map('KeyType', 'char', 'ValueType', 'char');
    
    while ~feof(fid)
        line = strtrim(fgetl(fid));
        parts = strsplit(line, ')');
        center = parts{1};
        orbiter = parts{2};
        
        if ~isKey(nodes, center)
            nodes(center) = uint64(0);
        end
        if ~isKey(nodes, orbiter)
            nodes(orbiter) = uint64(0);
        end
        
        parentMap(orbiter) = center;
    end
    fclose(fid);
    
    pathYOU = getPathToRoot(parentMap, 'YOU');
    pathSAN = getPathToRoot(parentMap, 'SAN');
    
    i = length(pathYOU);
    j = length(pathSAN);
    
    while i > 0 && j > 0 && strcmp(pathYOU{i}, pathSAN{j})
        i = i - 1;
        j = j - 1;
    end
    
    fprintf('%d\n', (i) + (j));
end

function path = getPathToRoot(parentMap, startNode)
    path = {};
    currentNode = startNode;
    while isKey(parentMap, currentNode)
        parent = parentMap(currentNode);
        path{end+1} = parent;
        currentNode = parent;
    end
end

main();
