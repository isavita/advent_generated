
function solve()
    % Initialize graph data structures
    nodeMap = containers.Map; % Maps node names (string) to integer IDs
    idCounter = 0;
    
    % Store edges temporarily to build adjacency matrix later
    % This avoids resizing the matrix repeatedly
    tempEdges = []; % Nx2 matrix of [id1, id2] pairs

    % To quickly check if a node starts with 't'
    tNodes = containers.Map; 

    % Read input file
    fid = fopen('input.txt', 'r');
    if fid == -1
        error('Could not open input.txt');
    end

    line = fgetl(fid);
    while ischar(line)
        parts = strsplit(line, '-');
        node1_str = parts{1};
        node2_str = parts{2};

        % Get/assign IDs for nodes
        if ~isKey(nodeMap, node1_str)
            idCounter = idCounter + 1;
            nodeMap(node1_str) = idCounter;
            if startsWith(node1_str, 't')
                tNodes(node1_str) = true;
            end
        end
        if ~isKey(nodeMap, node2_str)
            idCounter = idCounter + 1;
            nodeMap(node2_str) = idCounter;
            if startsWith(node2_str, 't')
                tNodes(node2_str) = true;
            end
        end

        id1 = nodeMap(node1_str);
        id2 = nodeMap(node2_str);

        % Store edges (undirected)
        tempEdges = [tempEdges; id1, id2; id2, id1]; % Add both directions
        
        line = fgetl(fid);
    end
    fclose(fid);

    % Create adjacency matrix
    numNodes = idCounter;
    % Use sparse matrix for efficiency, especially if the graph is large and sparse
    adjMatrix = sparse(tempEdges(:,1), tempEdges(:,2), 1, numNodes, numNodes);

    % Create a reverse map from ID to name for convenience
    % This is needed to get string names back from integer IDs
    nodeIDs = cell2mat(values(nodeMap));
    nodeNames = keys(nodeMap);
    idToNameMap = containers.Map(nodeIDs, nodeNames);

    count = 0;
    seenTriplets = containers.Map; % To store unique sorted triplets

    % Iterate through all combinations of three distinct nodes
    % Using integer IDs for efficiency in loops and matrix lookups
    % Optimization: Only iterate if adjMatrix(i,j) is true
    for i = 1:numNodes
        for j = i + 1:numNodes
            if adjMatrix(i, j) % Check if i and j are connected
                for k = j + 1:numNodes
                    % Check if j-k and i-k are connected
                    if adjMatrix(j, k) && adjMatrix(i, k) 
                        % Found a triangle (i, j, k)
                        
                        % Get original string names
                        c1_str = idToNameMap(i);
                        c2_str = idToNameMap(j);
                        c3_str = idToNameMap(k);

                        % Check if any node starts with 't'
                        % Using the pre-computed tNodes map for O(1) lookup
                        if isKey(tNodes, c1_str) || isKey(tNodes, c2_str) || isKey(tNodes, c3_str)
                            % Create a canonical representation for the triplet
                            % Sort the names alphabetically to ensure uniqueness regardless of order
                            tripletNames = sort({c1_str, c2_str, c3_str});
                            tripletKey = strjoin(tripletNames, '-'); % e.g., 'a-b-t_node'

                            if ~isKey(seenTriplets, tripletKey)
                                seenTriplets(tripletKey) = true;
                                count = count + 1;
                            end
                        end
                    end
                end
            end
        end
    end

    fprintf('%d\n', count);
end
