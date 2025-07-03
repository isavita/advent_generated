
function main()
    adj = containers.Map('KeyType', 'double', 'ValueType', 'any');
    
    file = fopen('input.txt', 'r');
    while ~feof(file)
        line = strtrim(fgets(file));
        parts = strsplit(line, ' <-> ');
        from_node = str2double(parts{1});
        to_nodes_str = strsplit(parts{2}, ', ');
        
        if ~isKey(adj, from_node)
            adj(from_node) = [];
        end
        
        for i = 1:length(to_nodes_str)
            to_node = str2double(to_nodes_str{i});
            adj(from_node) = [adj(from_node), to_node];
            
            if ~isKey(adj, to_node)
                adj(to_node) = [];
            end
            adj(to_node) = [adj(to_node), from_node];
        end
    end
    fclose(file);
    
    visited = containers.Map('KeyType', 'double', 'ValueType', 'logical');
    groups = 0;
    
    nodes = keys(adj);
    for i = 1:length(nodes)
        node = nodes{i};
        if ~isKey(visited, node) || ~visited(node)
            DFS(adj, visited, node);
            groups = groups + 1;
        end
    end
    
    disp(groups);
end

function DFS(adj, visited, node)
    visited(node) = true;
    neighbors = adj(node);
    for i = 1:length(neighbors)
        neighbor = neighbors(i);
        if ~isKey(visited, neighbor) || ~visited(neighbor)
            DFS(adj, visited, neighbor);
        end
    end
end

main();
