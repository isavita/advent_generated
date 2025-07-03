
function main()
    input_data = read_file("input.txt");
    result = cleaning_robot(input_data);
    disp(result);
end

function result = cleaning_robot(input_str)
    grid = strsplit(input_str, '\n');
    num_rows = length(grid);
    num_cols = length(grid{1});
    
    poi_locations = containers.Map('KeyType', 'char', 'ValueType', 'any');
    
    for r = 1:num_rows
        for c = 1:num_cols
            cell_val = grid{r}(c);
            if ~isempty(regexp(cell_val, '[0-9]', 'once'))
                poi_locations(cell_val) = [r, c];
            end
        end
    end
    
    num_pois = length(poi_locations);
    graph = zeros(num_pois, num_pois);
    
    poi_keys = keys(poi_locations);
    for i = 1:num_pois
        poi_char = poi_keys{i};
        start_pos = poi_locations(poi_char);
        distances = bfs_get_edge_weights(grid, start_pos, num_pois);
        graph(i, :) = distances;
    end
    
    visited = containers.Map('KeyType', 'double', 'ValueType', 'logical');
    visited(1) = true;
    result = dfs(graph, 1, visited, true);
end

function distances = bfs_get_edge_weights(grid, start_pos, num_pois)
    num_rows = length(grid);
    num_cols = length(grid{1});
    
    poi_to_distance = containers.Map('KeyType', 'char', 'ValueType', 'double');
    poi_to_distance('0') = 0; % Assuming '0' is always a POI and the starting point
    
    queue = struct('row', {}, 'col', {}, 'distance', {});
    queue(1) = struct('row', start_pos(1), 'col', start_pos(2), 'distance', 0);
    
    visited = containers.Map('KeyType', 'char', 'ValueType', 'logical');
    visited_key = sprintf('%d,%d', start_pos(1), start_pos(2));
    visited(visited_key) = true;
    
    head = 1;
    while head <= length(queue)
        current_node = queue(head);
        head = head + 1;
        
        r = current_node.row;
        c = current_node.col;
        dist = current_node.distance;
        
        cell_val = grid{r}(c);
        if ~isempty(regexp(cell_val, '[0-9]', 'once'))
            if ~isKey(poi_to_distance, cell_val) || poi_to_distance(cell_val) > dist
                poi_to_distance(cell_val) = dist;
            end
        end
        
        moves = [0, -1; 0, 1; 1, 0; -1, 0];
        for i = 1:4
            next_row = r + moves(i, 1);
            next_col = c + moves(i, 2);
            
            if next_row >= 1 && next_row <= num_rows && next_col >= 1 && next_col <= num_cols
                next_cell_val = grid{next_row}(next_col);
                if next_cell_val ~= '#'
                    visited_key = sprintf('%d,%d', next_row, next_col);
                    if ~isKey(visited, visited_key)
                        visited(visited_key) = true;
                        queue(end+1) = struct('row', next_row, 'col', next_col, 'distance', dist + 1);
                    end
                end
            end
        end
    end
    
    distances = zeros(1, num_pois);
    poi_keys = keys(poi_to_distance);
    for i = 1:length(poi_keys)
        poi_char = poi_keys{i};
        poi_index = str2double(poi_char) + 1;
        distances(poi_index) = poi_to_distance(poi_char);
    end
end

function min_dist = dfs(graph, current_index, visited, return_to_zero)
    num_pois = size(graph, 1);
    
    if length(visited) == num_pois
        if return_to_zero
            min_dist = graph(current_index, 1);
        else
            min_dist = 0;
        end
        return;
    end
    
    min_dist = inf;
    
    for next_index = 1:num_pois
        if ~isKey(visited, next_index)
            visited(next_index) = true;
            
            dist = graph(current_index, next_index) + dfs(graph, next_index, visited, return_to_zero);
            min_dist = min(min_dist, dist);
            
            remove(visited, next_index);
        end
    end
end

function content = read_file(path_from_caller)
    [~, ~, ~] = fileparts(mfilename('fullpath'));
    file_path = fullfile(pwd, path_from_caller);
    
    fid = fopen(file_path, 'r');
    content = fread(fid, '*char')';
    fclose(fid);
    
    content = strtrim(content);
end

