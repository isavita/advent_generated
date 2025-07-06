
function main()
    fid = fopen('input.txt', 'r');
    adj = containers.Map('KeyType', 'double', 'ValueType', 'any');
    
    while ~feof(fid)
        line = strtrim(fgetl(fid));
        parts = strsplit(line, ' <-> ');
        fromNode = str2double(parts{1});
        toNodes = strsplit(parts{2}, ', ');
        
        if ~isKey(adj, fromNode)
            adj(fromNode) = [];
        end
        
        for i = 1:length(toNodes)
            toNode = str2double(toNodes{i});
            adj(fromNode) = [adj(fromNode), toNode];
            
            if ~isKey(adj, toNode)
                adj(toNode) = [];
            end
            adj(toNode) = [adj(toNode), fromNode];
        end
    end
    fclose(fid);
    
    visited = containers.Map('KeyType', 'double', 'ValueType', 'logical');
    
    if isKey(adj, 0)
        dfs(0, adj, visited);
    end
    
    count = 0;
    keys = adj.keys;
    for i = 1:length(keys)
        if isKey(visited, keys{i}) && visited(keys{i})
            count = count + 1;
        end
    end
    
    disp(count);
end

function dfs(node, adj, visited)
    visited(node) = true;
    if isKey(adj, node)
        neighbors = adj(node);
        for i = 1:length(neighbors)
            neighbor = neighbors(i);
            if ~isKey(visited, neighbor) || ~visited(neighbor)
                dfs(neighbor, adj, visited);
            end
        end
    end
end

main();
