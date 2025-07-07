
function main()
    % Read input from input.txt
    try
        connections = readInput('input.txt');
    catch ME
        fprintf(2, 'Error reading input file: %s\n', ME.message);
        return;
    end

    % Build the adjacency list representation of the cave system
    adjList = buildAdjacencyList(connections);

    % Find all paths from 'start' to 'end'
    paths = findAllPaths(adjList, 'start', 'end');

    % Count the number of distinct paths
    numPaths = length(paths);

    % Print the output to standard output
    fprintf('Number of distinct paths: %d\n', numPaths);
end

function connections = readInput(filename)
    % Reads connections from a file.
    % Each line in the file should represent a connection, e.g., "start-A".
    % Returns a cell array of strings, where each string is a connection.
    fid = fopen(filename, 'r');
    if fid == -1
        error('File not found: %s', filename);
    end
    connections = textscan(fid, '%s', 'Delimiter', '\n');
    fclose(fid);
    connections = connections{1}; % textscan returns a cell array of cell arrays
end

function adjList = buildAdjacencyList(connections)
    % Builds an adjacency list representation of the cave system.
    % Input: connections - a cell array of strings, each representing a connection (e.g., "cave1-cave2").
    % Output: adjList - a map where keys are cave names and values are cell arrays of connected caves.
    adjList = containers.Map();

    for i = 1:length(connections)
        connection = strsplit(connections{i}, '-');
        cave1 = connection{1};
        cave2 = connection{2};

        % Add cave1 to adjList if not present
        if ~isKey(adjList, cave1)
            adjList(cave1) = {};
        end
        % Add cave2 to adjList if not present
        if ~isKey(adjList, cave2)
            adjList(cave2) = {};
        end

        % Add connections (undirected graph)
        adjList(cave1) = [adjList(cave1), {cave2}];
        adjList(cave2) = [adjList(cave2), {cave1}];
    end
end

function paths = findAllPaths(adjList, startNode, endNode)
    % Finds all distinct paths from startNode to endNode in the cave system.
    % This function uses a recursive depth-first search (DFS) approach.
    % It adheres to the rule of visiting small caves at most once.

    paths = {};
    % Initialize the DFS with the starting node and an empty visited set.
    % The visited set will store small caves visited in the current path.
    dfs(startNode, {startNode}, containers.Map());
    
    function dfs(currentNode, currentPath, visitedSmallCaves)
        % Base case: If the current node is the end node, we've found a path.
        if strcmp(currentNode, endNode)
            paths{end+1} = currentPath;
            return;
        end

        % Mark the current node as visited if it's a small cave.
        % Big caves (uppercase) can be visited multiple times.
        if isSmallCave(currentNode)
            % Check if this small cave has already been visited in this path.
            % If it has, this path is invalid according to the rules.
            if isKey(visitedSmallCaves, currentNode)
                return;
            end
            % Mark the current small cave as visited for this path.
            visitedSmallCaves(currentNode) = true;
        end

        % Explore neighbors
        neighbors = adjList(currentNode);
        for i = 1:length(neighbors)
            neighbor = neighbors{i};

            % Recursively call DFS for the neighbor, passing a copy of the
            % current path and visited small caves.
            % We pass copies to ensure that each recursive call has its own
            % state, preventing interference between different branches of the search.
            dfs(neighbor, [currentPath, {neighbor}], copyMap(visitedSmallCaves));
        end
    end
end

function isSmall = isSmallCave(caveName)
    % Determines if a cave is small (lowercase) or big (uppercase).
    % Returns true if the cave name is entirely lowercase, false otherwise.
    isSmall = all(islower(caveName));
end

function copiedMap = copyMap(originalMap)
    % Creates a deep copy of a containers.Map object.
    % This is crucial for DFS to maintain independent visited states for each branch.
    keys = originalMap.keys;
    values = originalMap.values;
    copiedMap = containers.Map();
    for i = 1:length(keys)
        copiedMap(keys{i}) = values{i};
    end
end

% Call the main function to start the program execution.
% This is the entry point of the MATLAB script.
if nargin == 0
    main();
end
