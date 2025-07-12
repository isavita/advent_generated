
function main()
    adj = containers.Map('KeyType', 'char', 'ValueType', 'any');
    nodeMap = containers.Map('KeyType', 'char', 'ValueType', 'double');
    nodeCount = 0;

    fileId = fopen('input.txt', 'r');
    lines = textscan(fileId, '%s', 'Delimiter', '-', 'WhiteSpace', '\n');
    fclose(fileId);

    for i = 1:2:length(lines{1})
        from = lines{1}{i};
        to = lines{1}{i+1};

        if ~isKey(nodeMap, from)
            nodeCount = nodeCount + 1;
            nodeMap(from) = nodeCount;
            adj(from) = {};
        end
        if ~isKey(nodeMap, to)
            nodeCount = nodeCount + 1;
            nodeMap(to) = nodeCount;
            adj(to) = {};
        end

        adj(from) = [adj(from), {to}];
        adj(to) = [adj(to), {from}];
    end

    visited = containers.Map('KeyType', 'char', 'ValueType', 'double');
    for i = 1:nodeCount
        nodeName = nodeMap.keys{i};
        visited(nodeName) = 0;
    end

    disp(walk('start', adj, visited, false));
end

function count = walk(current, adj, visited, doubleUsed)
    if strcmp(current, 'end')
        count = 1;
        return;
    end

    visited(current) = visited(current) + 1;
    count = 0;

    neighbors = adj(current);
    for i = 1:length(neighbors)
        next = neighbors{i};

        if strcmp(next, 'start')
            continue;
        end

        isLower = all(isstrprop(next, 'lower'));

        if isLower && visited(next) > 0
            if doubleUsed
                continue;
            else
                newDoubleUsed = true;
            end
        else
            newDoubleUsed = doubleUsed;
        end

        count = count + walk(next, adj, visited, newDoubleUsed);
    end

    visited(current) = visited(current) - 1;
end

main();
