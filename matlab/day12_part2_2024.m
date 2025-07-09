
function solve()
    % Read input.txt
    fid = fopen('input.txt', 'r');
    lines = textscan(fid, '%s', 'Delimiter', '\n', 'Whitespace', '');
    fclose(fid);
    lines = lines{1};

    % Filter empty lines and convert to char array
    graph_cell = lines(~cellfun('isempty', lines));
    H = length(graph_cell);
    if H == 0
        disp(0);
        return;
    end
    W = length(graph_cell{1});
    graph = char(graph_cell);

    % Define possible moves (dy, dx) or (row_change, col_change)
    moves = [-1, 0; 0, -1; 1, 0; 0, 1]; 

    total_sum = 0;

    % Create a mutable copy of the graph for BFS processing
    current_graph = graph;

    for y = 1:H
        for x = 1:W
            if current_graph(y, x) == '.'
                continue;
            end

            area = 0;
            target_char = current_graph(y, x);
            
            % Initialize 'side' map to store outer boundary points
            side = containers.Map();
            side('left') = {}; 
            side('up') = {};   
            side('right') = {};
            side('down') = {}; 

            % BFS queue: { {col, row, label}, ... }
            q = {{x, y, ''}}; 
            q_idx = 1; % Pointer for dequeue operation

            % Visited matrix for the current BFS component
            bfs_visited = false(H, W); 

            while q_idx <= length(q)
                current_item = q{q_idx};
                cx = current_item{1}; % current column
                cy = current_item{2}; % current row
                label = current_item{3}; % label indicating direction from parent
                q_idx = q_idx + 1; % Dequeue

                % Check if current coordinates are out of bounds
                is_oob = (cx < 1 || cx > W || cy < 1 || cy > H);

                if is_oob
                    add_outer(label, side, cx, cy);
                    continue;
                end

                % If this cell has already been visited in the current BFS, skip
                if bfs_visited(cy, cx)
                    continue; 
                end

                % If the character at (cy, cx) is not the target character
                if current_graph(cy, cx) ~= target_char
                    % This is an in-bounds "outer" point. Add if reached from a target cell.
                    if ~isempty(label) 
                        add_outer(label, side, cx, cy);
                    end
                    continue;
                end

                % This is a target character cell: process it
                bfs_visited(cy, cx) = true; % Mark as visited for this BFS
                area = area + 1;
                current_graph(cy, cx) = '.'; % Mark as globally visited (removed from future BFS)

                % Add neighbors to the queue
                for k = 1:size(moves, 1)
                    dy = moves(k, 1); % row change
                    dx = moves(k, 2); % col change
                    nx = cx + dx;
                    ny = cy + dy;
                    
                    new_label = get_label(dx, dy);
                    q{end+1} = {nx, ny, new_label}; % Enqueue
                end
            end

            outer = count_outer(side);
            total_sum = total_sum + area * outer;
        end
    end

    disp(total_sum);
end

function add_outer(label, side_map, x, y)
    % Adds a coordinate pair to the appropriate list in the side_map.
    % side_map is a containers.Map, which is a handle object, so modifications persist.
    if strcmp(label, 'up') || strcmp(label, 'down')
        key_val = {y, x}; % Store as {row, col} for vertical boundaries
    else % 'left' or 'right'
        key_val = {x, y}; % Store as {col, row} for horizontal boundaries
    end
    
    current_list = side_map(label);
    current_list{end+1} = key_val;
    side_map(label) = current_list;
end

function outer_count = count_outer(side_map)
    % Counts distinct "segments" of outer boundary points.
    % Segments are defined by horizontal adjacency of the stored coordinate pairs.
    outer_count = 0;
    labels = side_map.keys;
    for k = 1:length(labels)
        label = labels{k};
        current_keys_cell = side_map(label); 
        
        if isempty(current_keys_cell)
            continue;
        end

        % Convert cell array of {c1, c2} pairs to a numeric matrix [c1, c2]
        current_keys_mat = zeros(length(current_keys_cell), 2);
        for i = 1:length(current_keys_cell)
            current_keys_mat(i, :) = cell2mat(current_keys_cell{i});
        end

        % Remove duplicate rows and sort for efficient segment counting
        sorted_keys = unique(current_keys_mat, 'rows'); 
        sorted_keys = sortrows(sorted_keys);

        num_segments = 0;
        if ~isempty(sorted_keys)
            num_segments = 1; % Start with one segment if points exist
            for i = 2:size(sorted_keys, 1)
                % A new segment starts if:
                % 1. The first coordinate changes (e.g., new row/column of points).
                % 2. The first coordinate is the same, but the second coordinate is not
                %    consecutive to the previous point (i.e., a gap exists).
                if sorted_keys(i, 1) ~= sorted_keys(i-1, 1) || ...
                   sorted_keys(i, 2) ~= sorted_keys(i-1, 2) + 1
                    num_segments = num_segments + 1;
                end
            end
        end
        outer_count = outer_count + num_segments;
    end
end

function label_str = get_label(dx, dy)
    % Returns a string label based on the direction vector (dx, dy).
    if dx == -1
        label_str = 'left';
    elseif dx == 1
        label_str = 'right';
    elseif dy == -1
        label_str = 'up';
    else % dy == 1
        label_str = 'down';
    end
end

% Call the main function to execute the program
solve();
