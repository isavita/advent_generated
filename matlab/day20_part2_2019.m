
function main()
    % Constants
    MAX_H = 150;
    MAX_W = 150;
    MAX_LEVEL = 50;
    MAX_QUEUE_SIZE = 1000000;

    % Read input
    [grid, height, width] = read_input_matlab('input.txt', MAX_H, MAX_W);

    % Find and map portals
    [start_pos, end_pos, portal_lookup_map] = find_and_map_portals_matlab(grid, height, width);

    % BFS
    result = bfs_matlab(grid, height, width, start_pos, end_pos, portal_lookup_map, MAX_LEVEL, MAX_QUEUE_SIZE);

    fprintf('%d\n', result);
end

function [grid, height, width] = read_input_matlab(filename, max_h, max_w)
    fid = fopen(filename, 'r');
    if fid == -1
        error('Error opening file: %s', filename);
    end

    lines = cell(max_h, 1);
    h = 0;
    max_w_found = 0;
    while ~feof(fid)
        line = fgetl(fid);
        if ischar(line)
            h = h + 1;
            if h > max_h
                h = max_h;
                break;
            end
            lines{h} = line;
            max_w_found = max(max_w_found, length(line));
        end
    end
    fclose(fid);

    height = h;
    width = max_w_found;
    
    lines = lines(1:height);

    grid = char(zeros(height, width));
    for i = 1:height
        current_line = lines{i};
        grid(i, 1:length(current_line)) = current_line;
        if length(current_line) < width
            grid(i, (length(current_line)+1):width) = ' ';
        end
    end
end

function [start_pos, end_pos, portal_lookup_map] = find_and_map_portals_matlab(grid, height, width)
    found_portals_temp = struct('label', cell(1,1), 'pos', [0,0]);
    found_count = 0;

    start_pos = [0,0];
    end_pos = [0,0];
    portal_lookup_map = containers.Map;

    for y_idx = 1:height
        for x_idx = 1:width
            char_val = grid(y_idx, x_idx);
            if isletter(char_val) && isupper(char_val)
                label = char([0,0]);
                pos = [-1,-1]; % C-style 0-based coordinates [x,y]

                if x_idx + 1 <= width && isletter(grid(y_idx, x_idx+1)) && isupper(grid(y_idx, x_idx+1))
                    label(1) = char_val;
                    label(2) = grid(y_idx, x_idx+1);
                    
                    if x_idx > 1 && grid(y_idx, x_idx-1) == '.'
                        pos = [x_idx-2, y_idx-1];
                    elseif x_idx + 2 <= width && grid(y_idx, x_idx+2) == '.'
                        pos = [x_idx+1, y_idx-1];
                    end
                elseif y_idx + 1 <= height && isletter(grid(y_idx+1, x_idx)) && isupper(grid(y_idx+1, x_idx))
                    label(1) = char_val;
                    label(2) = grid(y_idx+1, x_idx);
                    
                    if y_idx > 1 && grid(y_idx-1, x_idx) == '.'
                        pos = [x_idx-1, y_idx-2];
                    elseif y_idx + 2 <= height && grid(y_idx+2, x_idx) == '.'
                        pos = [x_idx-1, y_idx+1];
                    end
                end

                if pos(1) ~= -1
                    found_count = found_count + 1;
                    if found_count > length(found_portals_temp)
                        found_portals_temp(found_count + 10).label = '';
                    end
                    found_portals_temp(found_count).label = label;
                    found_portals_temp(found_count).pos = pos;
                end
            end
        end
    end

    if found_count > 0
        found_portals_temp = found_portals_temp(1:found_count);
    else
        found_portals_temp = struct('label', {}, 'pos', {});
    end

    paired_indices = false(1, found_count);
    for i = 1:found_count
        if paired_indices(i), continue; end

        current_label = found_portals_temp(i).label;
        current_pos = found_portals_temp(i).pos;

        if strcmp(current_label, 'AA')
            start_pos = current_pos;
        elseif strcmp(current_label, 'ZZ')
            end_pos = current_pos;
        else
            for j = i + 1:found_count
                if ~paired_indices(j) && strcmp(current_label, found_portals_temp(j).label)
                    other_pos = found_portals_temp(j).pos;

                    is_outer_i = is_outer_matlab(current_pos, width, height);
                    is_outer_j = is_outer_matlab(other_pos, width, height);

                    portal_lookup_map(sprintf('%d_%d', current_pos(1), current_pos(2))) = ...
                        struct('to_x', other_pos(1), 'to_y', other_pos(2), 'is_outer', is_outer_i);
                    portal_lookup_map(sprintf('%d_%d', other_pos(1), other_pos(2))) = ...
                        struct('to_x', current_pos(1), 'to_y', current_pos(2), 'is_outer', is_outer_j);

                    paired_indices(i) = true;
                    paired_indices(j) = true;
                    break;
                end
            end
        end
    end
end

function outer = is_outer_matlab(p, width, height)
    outer = p(1) <= 2 || p(2) <= 2 || p(1) >= width - 3 || p(2) >= height - 3;
end

function steps = bfs_matlab(grid, height, width, start_pos, end_pos, portal_lookup_map, max_level, max_queue_size)
    visited = false(height, width, max_level);

    queue = zeros(max_queue_size, 4);
    q_head = 1;
    q_tail = 1;

    queue(q_tail, :) = [start_pos(1), start_pos(2), 0, 0];
    visited(start_pos(2)+1, start_pos(1)+1, 0+1) = true;
    q_tail = q_tail + 1;

    dx = [0, 0, 1, -1];
    dy = [1, -1, 0, 0];

    while q_head < q_tail
        current = queue(q_head, :);
        q_head = q_head + 1;

        curr_x = current(1);
        curr_y = current(2);
        curr_level = current(3);
        curr_steps = current(4);

        if curr_x == end_pos(1) && curr_y == end_pos(2) && curr_level == 0
            steps = curr_steps;
            return;
        end

        for i = 1:4
            nx = curr_x + dx(i);
            ny = curr_y + dy(i);

            if nx >= 0 && nx < width && ny >= 0 && ny < height
                if grid(ny+1, nx+1) == '.'
                    if curr_level < max_level && ~visited(ny+1, nx+1, curr_level+1)
                        visited(ny+1, nx+1, curr_level+1) = true;
                        if q_tail > max_queue_size
                            error('Queue overflow: Increase MAX_QUEUE_SIZE');
                        end
                        queue(q_tail, :) = [nx, ny, curr_level, curr_steps + 1];
                        q_tail = q_tail + 1;
                    end
                end
            end
        end

        portal_key = sprintf('%d_%d', curr_x, curr_y);
        if portal_lookup_map.isKey(portal_key)
            link = portal_lookup_map(portal_key);
            
            new_level = curr_level + (link.is_outer * -1 + (1 - link.is_outer) * 1);
            
            if new_level >= 0 && new_level < max_level
                target_x = link.to_x;
                target_y = link.to_y;

                if ~visited(target_y+1, target_x+1, new_level+1)
                    visited(target_y+1, target_x+1, new_level+1) = true;
                    if q_tail > max_queue_size
                        error('Queue overflow: Increase MAX_QUEUE_SIZE');
                    end
                    queue(q_tail, :) = [target_x, target_y, new_level, curr_steps + 1];
                    q_tail = q_tail + 1;
                end
            end
        end
    end

    steps = -1;
end
