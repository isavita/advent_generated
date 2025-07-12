
% main.m
function main()
    % Read input from file
    fid = fopen('input.txt', 'r');
    if fid == -1
        error('Error opening input.txt');
    end
    
    lines = textscan(fid, '%s', 'Delimiter', '\n', 'Whitespace', '');
    fclose(fid);
    
    input_lines = lines{1};
    
    % Convert cell array of strings to char matrix
    height = numel(input_lines);
    width = length(input_lines{1});
    grid_data = char(zeros(height, width));
    for i = 1:height
        grid_data(i, :) = input_lines{i};
    end
    
    result = solve(grid_data);
    fprintf('%d\n', result);
end

% solve.m
function max_dist = solve(grid_data)
    [height, width] = size(grid_data);
    
    % C coordinates are 0-indexed [x, y]
    % MATLAB coordinates are 1-indexed [x, y] (column, row)
    % C start: {1, 0} -> MATLAB start: [1+1, 0+1] = [2, 1]
    % C end: {width - 2, height - 1} -> MATLAB end: [width-2+1, height-1+1] = [width-1, height]
    start_coord = [2, 1]; 
    end_coord = [width - 1, height]; 
    
    % Build graph
    [adj, vertex_map, start_idx, end_idx] = getGraph(grid_data, start_coord, end_coord);
    
    % Find longest path using DFS
    num_vertices = numel(keys(vertex_map));
    seen = false(1, num_vertices);
    
    max_dist = getMaxDistanceDFS(adj, start_idx, end_idx, seen);
end

% getGraph.m
function [adj, vertex_map, start_idx, end_idx] = getGraph(grid_data, start_coord, end_coord)
    [height, width] = size(grid_data);
    
    vertices_list = {};
    vertex_map_temp = containers.Map(); 
    
    function add_vertex(coord)
        coord_str = coord_to_str(coord);
        if ~isKey(vertex_map_temp, coord_str)
            vertices_list{end+1} = coord;
            vertex_map_temp(coord_str) = numel(vertices_list); 
        end
    end
    
    add_vertex(start_coord);
    add_vertex(end_coord);
    
    neighbors_dirs = [0, -1; 0, 1; -1, 0; 1, 0]; 
    
    for y = 1:height
        for x = 1:width
            if grid_data(y, x) == '.' 
                current_coord = [x, y];
                valid_neighbors_count = 0;
                for i = 1:size(neighbors_dirs, 1)
                    neighbor_coord = add_coords(current_coord, neighbors_dirs(i,:));
                    if isValid(grid_data, neighbor_coord)
                        valid_neighbors_count = valid_neighbors_count + 1;
                    end
                end
                if valid_neighbors_count > 2
                    add_vertex(current_coord);
                end
            end
        end
    end
    
    all_vertices_coords = cell2mat(vertices_list');
    all_vertices_coords = unique(all_vertices_coords, 'rows', 'stable'); 
    
    vertex_map = containers.Map();
    for i = 1:size(all_vertices_coords, 1)
        vertex_map(coord_to_str(all_vertices_coords(i,:))) = i;
    end
    
    start_idx = vertex_map(coord_to_str(start_coord));
    end_idx = vertex_map(coord_to_str(end_coord));
    
    num_vertices = size(all_vertices_coords, 1);
    adj = cell(num_vertices, 1); 
    
    for i = 1:num_vertices
        current_vertex_coord = all_vertices_coords(i,:);
        
        edges_from_current = getEdgesBFS(grid_data, current_vertex_coord, vertex_map, @isValidWithSlopes);
        
        if ~isempty(edges_from_current)
            adj{i} = zeros(size(edges_from_current, 1), 2);
            for j = 1:size(edges_from_current, 1)
                target_coord_str = coord_to_str(edges_from_current(j, 1:2));
                target_idx = vertex_map(target_coord_str);
                weight = edges_from_current(j, 3);
                adj{i}(j, :) = [target_idx, weight];
            end
        end
    end
end

% getEdgesBFS.m
function edges_found = getEdgesBFS(grid_data, start_coord, vertex_map, isValidFunc)
    queue = {start_coord};
    distances = containers.Map(); 
    reached = containers.Map();   
    
    distances(coord_to_str(start_coord)) = 0;
    reached(coord_to_str(start_coord)) = true;
    
    edges_found = []; 
    
    neighbors_dirs = [0, -1; 0, 1; -1, 0; 1, 0]; 
    
    head = 1; 
    while head <= numel(queue)
        current_coord = queue{head};
        head = head + 1; 
        
        current_dist = distances(coord_to_str(current_coord));
        
        if ~isequal(current_coord, start_coord) && isKey(vertex_map, coord_to_str(current_coord))
            edges_found = [edges_found; current_coord, current_dist]; %#ok<AGROW>
            continue; 
        end
        
        for i = 1:size(neighbors_dirs, 1)
            dir = neighbors_dirs(i,:);
            next_coord = add_coords(current_coord, dir);
            next_coord_str = coord_to_str(next_coord);
            
            if isValidFunc(grid_data, next_coord, dir) && ~isKey(reached, next_coord_str)
                queue{end+1} = next_coord; 
                reached(next_coord_str) = true;
                distances(next_coord_str) = current_dist + 1;
            end
        end
    end
end

% getMaxDistanceDFS.m
function maxi = getMaxDistanceDFS(adj, current_idx, end_idx, seen)
    if current_idx == end_idx
        maxi = 0;
        return;
    end
    
    maxi = -inf; 
    seen(current_idx) = true; 
    
    if ~isempty(adj{current_idx})
        for i = 1:size(adj{current_idx}, 1)
            next_idx = adj{current_idx}(i, 1);
            weight = adj{current_idx}(i, 2);
            
            if ~seen(next_idx)
                dist = getMaxDistanceDFS(adj, next_idx, end_idx, seen);
                
                if dist ~= -inf 
                    maxi = max(maxi, dist + weight);
                end
            end
        end
    end
    
    seen(current_idx) = false; 
    
    if maxi == -inf
        maxi = -1; 
    end
end

% isValid.m
function res = isValid(grid_data, coord)
    [height, width] = size(grid_data);
    x = coord(1); 
    y = coord(2); 
    res = x >= 1 && x <= width && y >= 1 && y <= height && grid_data(y, x) ~= '#';
end

% isValidWithSlopes.m
function res = isValidWithSlopes(grid_data, coord, dir)
    res = isValid(grid_data, coord);
    if ~res
        return;
    end
    
    x = coord(1);
    y = coord(2);
    
    c = grid_data(y, x);
    
    if c == '^'
        res = dir(2) == -1; 
    elseif c == 'v'
        res = dir(2) == 1;  
    elseif c == '<'
        res = dir(1) == -1; 
    elseif c == '>'
        res = dir(1) == 1;  
    else
        res = true; 
    end
end

% add_coords.m
function res = add_coords(c1, c2)
    res = c1 + c2;
end

% coord_to_str.m
function s = coord_to_str(coord)
    s = sprintf('%d_%d', coord(1), coord(2));
end
