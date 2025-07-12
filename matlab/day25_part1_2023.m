
function main()

persistent vertex_map_global;
persistent next_vertex_id_global;
persistent edges_data_global;
persistent next_edge_id_global;

if isempty(vertex_map_global)
    vertex_map_global = containers.Map;
    next_vertex_id_global = 0;
    edges_data_global = [];
    next_edge_id_global = 0;
end

fid = fopen('input.txt', 'r');
if fid == -1
    error('Error opening input.txt');
end

tline = fgetl(fid);
while ischar(tline)
    parts = strsplit(tline, ':');
    vertex_str = strtrim(parts{1});
    neighbors_str = strtrim(parts{2});

    [u_id, vertex_map_global, next_vertex_id_global] = get_vertex_id_helper(vertex_str, vertex_map_global, next_vertex_id_global);

    neighbor_tokens = strsplit(neighbors_str, ' ');
    for i = 1:length(neighbor_tokens)
        neighbor_tok = neighbor_tokens{i};
        [v_id, vertex_map_global, next_vertex_id_global] = get_vertex_id_helper(neighbor_tok, vertex_map_global, next_vertex_id_global);

        edges_data_global = [edges_data_global; u_id, v_id, next_edge_id_global, 1];
        next_edge_id_global = next_edge_id_global + 1;
    end

    tline = fgetl(fid);
end
fclose(fid);

vertex_count = vertex_map_global.Count;

adj_list = cell(vertex_count, 1);
for i = 1:size(edges_data_global, 1)
    u = edges_data_global(i, 1);
    v = edges_data_global(i, 2);
    edge_id = edges_data_global(i, 3);

    adj_list{u + 1} = [adj_list{u + 1}; v, edge_id];
    adj_list{v + 1} = [adj_list{v + 1}; u, edge_id];
end

result = solve_problem(vertex_count, edges_data_global, adj_list);

fprintf('%d\n', result);

end

function [id, vertex_map_out, next_id_out] = get_vertex_id_helper(name, vertex_map_in, current_next_id)
    if vertex_map_in.isKey(name)
        id = vertex_map_in(name);
        vertex_map_out = vertex_map_in;
        next_id_out = current_next_id;
    else
        id = current_next_id;
        vertex_map_in(name) = id;
        vertex_map_out = vertex_map_in;
        next_id_out = current_next_id + 1;
    end
end

function [found, parent_edge] = bfs(start_node, end_node, vertex_count, edges_data, adj_list)
    parent_edge = -1 * ones(1, vertex_count);
    visited = false(1, vertex_count);
    queue = zeros(1, vertex_count);

    head = 1;
    tail = 1;

    queue(tail) = start_node;
    tail = tail + 1;
    visited(start_node + 1) = true;

    found = false;

    while head < tail
        current = queue(head);
        head = head + 1;

        if end_node ~= -1 && current == end_node
            found = true;
            break;
        end

        if (current + 1 <= length(adj_list)) && ~isempty(adj_list{current + 1})
            neighbors_info = adj_list{current + 1};
            for i = 1:size(neighbors_info, 1)
                neighbor = neighbors_info(i, 1);
                edge_id = neighbors_info(i, 2);

                edge_active = edges_data(edge_id + 1, 4);

                if edge_active && ~visited(neighbor + 1)
                    visited(neighbor + 1) = true;
                    parent_edge(neighbor + 1) = edge_id;
                    queue(tail) = neighbor;
                    tail = tail + 1;
                end
            end
        end
    end

    if end_node == -1
       found = true;
    end

    if end_node ~= -1 && ~found
         parent_edge = [];
    end
end

function path_edges = reconstruct_path(start_node, end_node, parent_edge, edges_data)
    path_edges = [];
    current = end_node;

    while current ~= start_node && current + 1 <= length(parent_edge) && parent_edge(current + 1) ~= -1
        edge_id = parent_edge(current + 1);
        path_edges = [path_edges, edge_id];

        u = edges_data(edge_id + 1, 1);
        v = edges_data(edge_id + 1, 2);

        if current == u
            current = v;
        else
            current = u;
        end
    end
    path_edges = fliplr(path_edges);
end

function result = solve_problem(vertex_count, initial_edges_data, adj_list)
    min_cut = 3;
    source_node = 0;

    cut_component_size = -1;

    edges_data_mutable = initial_edges_data;

    for end_node = 1:(vertex_count - 1)
        edges_data_mutable(:, 4) = 1;

        paths_found = 0;
        for k = 1:min_cut
            [bfs_found, parent_edge] = bfs(source_node, end_node, vertex_count, edges_data_mutable, adj_list);

            if ~bfs_found
                break;
            end

            path_edges = reconstruct_path(source_node, end_node, parent_edge, edges_data_mutable);

            for i = 1:length(path_edges)
                edge_id_to_disable = path_edges(i);
                edges_data_mutable(edge_id_to_disable + 1, 4) = 0;
            end
            paths_found = paths_found + 1;
        end

        if paths_found == min_cut
            [disconnected_check, ~] = bfs(source_node, end_node, vertex_count, edges_data_mutable, adj_list);

            if ~disconnected_check
                [~, component_parent_edge] = bfs(source_node, -1, vertex_count, edges_data_mutable, adj_list);

                count = sum(component_parent_edge ~= -1);
                count = count + 1;

                cut_component_size = count;
                break;
            end
        end
    end

    if cut_component_size ~= -1
        size1 = cut_component_size;
        size2 = vertex_count - size1;
        result = size1 * size2;
    else
        result = -1;
    end
end
