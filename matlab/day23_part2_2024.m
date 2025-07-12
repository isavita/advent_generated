
function main()
    % Constants
    MAX_NODES = 2048; % Max number of unique computer names.
    FILENAME = 'input.txt';

    % Initialize variables (these will be accessible to nested functions)
    adj = false(MAX_NODES, MAX_NODES); % Adjacency matrix (logical for memory efficiency)
    node_names = cell(MAX_NODES, 1);   % Stores actual names of nodes
    node_count = 0;                    % Current number of unique nodes discovered
    node_name_map = containers.Map('KeyType', 'char', 'ValueType', 'double'); % Maps name to ID

    % Variables to store the result of the clique finding
    max_clique_nodes = []; % Stores node indices of the largest clique found
    max_clique_size = 0;   % Stores the size of the largest clique found

    % --- Nested Helper Functions ---

    % get_node_id: Gets the unique integer ID for a node name.
    % If the name is new, assigns a new ID and stores the name.
    % Uses a containers.Map for efficient lookup (hash map).
    function id = get_node_id_nested(name)
        if isKey(node_name_map, name)
            id = node_name_map(name);
        else
            if node_count >= MAX_NODES
                error('Maximum node limit (%d) exceeded trying to add ''%s''.', MAX_NODES, name);
            end
            node_count = node_count + 1;
            id = node_count; % MATLAB uses 1-based indexing
            node_names{id} = name;
            node_name_map(name) = id;
        end
    end

    % find_max_clique_recursive: Recursive function to find the maximum clique
    % using backtracking with pruning (Bron-Kerbosch algorithm variant).
    function find_max_clique_recursive_nested(R, P)
        % R: Array storing node indices currently in the potential clique being built.
        % P: Array storing node indices that are candidates to extend R.

        % Base Case 1: No candidates left to extend the current clique R.
        % R is now a maximal clique.
        if isempty(P)
            if length(R) > max_clique_size
                max_clique_size = length(R);
                max_clique_nodes = R;
            end
            return;
        end

        % Base Case 2: Pruning optimization.
        % If the current clique size plus all remaining candidates cannot possibly
        % form a clique larger than the best one already found, abandon this path.
        if length(R) + length(P) <= max_clique_size
            return;
        end

        % --- Recursive Step ---
        % Iterate through candidate nodes 'v' in P.
        % Create a copy of P for iteration to avoid issues with modifying P
        % while iterating, though MATLAB's copy-on-write often handles this.
        P_copy = P;

        for i = 1:length(P_copy)
            v = P_copy(i); % Select the i-th candidate 'v'.

            % 1. Form the new potential clique R' = R union {v}.
            next_R = [R, v];

            % 2. Form the new candidate set P' for the recursive call.
            %    P' consists of nodes in P (specifically P_copy(j) where j > i)
            %    that are connected to 'v'.
            
            % Get candidates from P_copy that are after 'v' in the current iteration.
            remaining_P_candidates = P_copy(i+1:end);
            
            % Find which of these remaining candidates are neighbors of 'v'.
            % adj(v, remaining_P_candidates) returns a logical array indicating
            % connections from 'v' to each node in 'remaining_P_candidates'.
            is_connected_to_v = adj(v, remaining_P_candidates);
            
            % Select only those candidates that are connected to 'v'.
            next_P = remaining_P_candidates(is_connected_to_v);

            % 3. Make the recursive call to explore cliques extending R'.
            find_max_clique_recursive_nested(next_R, next_P);
        end
    end

    % --- Main Logic ---

    % Initialize adjacency matrix to all false (no connections initially).
    % This is done implicitly by `adj = false(MAX_NODES, MAX_NODES);`

    % Read Input and Build Graph
    fid = fopen(FILENAME, 'r');
    if fid == -1
        error('Error opening input file: %s. Ensure ''%s'' exists in the current directory.', FILENAME, FILENAME);
    end

    while ~feof(fid)
        line = fgetl(fid);
        if ~ischar(line), break; end % End of file or error

        % Remove leading/trailing whitespace and check if line is empty
        line = strtrim(line);
        if isempty(line), continue; end

        % Find the hyphen separating the names.
        hyphen_idx = strfind(line, '-');
        if isempty(hyphen_idx)
            % Ignore blank lines silently, warn about other malformed lines.
            if ~isempty(line)
                warning('Malformed line (no hyphen): %s', line);
            end
            continue;
        end

        % Split the line into two names.
        name1 = strtrim(line(1:hyphen_idx-1));
        name2 = strtrim(line(hyphen_idx+1:end));

        % Skip if names are empty after parsing
        if isempty(name1) || isempty(name2)
            continue;
        end

        % Get unique integer IDs for each name, adding them if new.
        try
            id1 = get_node_id_nested(name1);
            id2 = get_node_id_nested(name2);
        catch ME
            fclose(fid);
            rethrow(ME); % Re-throw the error from get_node_id_nested
        end

        % If IDs are valid (within bounds and different), mark the connection.
        if id1 > 0 && id2 > 0
            if id1 ~= id2 % Ensure no self-loops are added
                adj(id1, id2) = true;
                adj(id2, id1) = true; % Undirected graph
            end
        end
    end
    fclose(fid);

    % Find the Maximum Clique
    if node_count == 0
        fprintf('No nodes found in input.\n');
        return;
    end

    % Initial candidates (P) are all nodes (1 to node_count).
    initial_P = 1:node_count;
    initial_R = []; % Empty initial clique

    % Start the recursive search.
    find_max_clique_recursive_nested(initial_R, initial_P);

    % Prepare and Print Output
    if max_clique_size == 0
        fprintf('No clique found (graph might be empty or have no edges).\n');
        return;
    end

    % Get the names of the nodes in the maximum clique.
    result_names = cell(1, max_clique_size);
    for i = 1:max_clique_size
        node_idx = max_clique_nodes(i);
        % Basic bounds check for safety before accessing node_names
        if node_idx >= 1 && node_idx <= node_count
            result_names{i} = node_names{node_idx};
        else
            error('Error: Invalid node index %d in max clique array.', node_idx);
        end
    end

    % Sort the names alphabetically.
    result_names = sort(result_names);

    % Print the sorted names, joined by commas, to standard output.
    fprintf('%s\n', strjoin(result_names, ','));

end
