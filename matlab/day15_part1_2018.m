
function main()
    fid = fopen('input.txt', 'r');
    data = textscan(fid, '%s', 'Delimiter', '\n', 'Whitespace', '');
    fclose(fid);
    map = char(data{1});
    
    answer = solve(map);
    fprintf('%d\n', answer);
end

function answer = solve(map)
    units = get_units(map);
    rounds = 0;
    
    while true
        [~, sort_idx] = sortrows([[units.y]', [units.x]']);
        turn_order = sort_idx([units(sort_idx).hp] > 0);

        round_completed = true;
        for i = 1:length(turn_order)
            u_idx = turn_order(i);
            if units(u_idx).hp <= 0
                continue;
            end

            is_elf = units(u_idx).type == 'E';
            enemy_type = 'G';
            if is_elf, enemy_type = 'G'; else, enemy_type = 'E'; end
            
            enemy_indices = find([units.type] == enemy_type & [units.hp] > 0);
            if isempty(enemy_indices)
                round_completed = false;
                break;
            end

            % Move
            adj_enemies = find_adjacent_enemies(u_idx, units, enemy_indices);
            if isempty(adj_enemies)
                [map, units] = move_unit(u_idx, units, map, enemy_indices);
            end
            
            % Attack
            adj_enemies = find_adjacent_enemies(u_idx, units, enemy_indices);
            if ~isempty(adj_enemies)
                enemy_hp = [units(adj_enemies).hp];
                min_hp = min(enemy_hp);
                targets = adj_enemies(enemy_hp == min_hp);
                
                [~, sort_idx] = sortrows([[units(targets).y]', [units(targets).x]']);
                target_idx = targets(sort_idx(1));
                
                units(target_idx).hp = units(target_idx).hp - units(u_idx).power;
                if units(target_idx).hp <= 0
                    map(units(target_idx).y, units(target_idx).x) = '.';
                end
            end
        end
        
        if ~round_completed
            break;
        end
        rounds = rounds + 1;
    end
    
    total_hp = sum([units([units.hp] > 0).hp]);
    answer = rounds * total_hp;
end

function units = get_units(map)
    [y_e, x_e] = find(map == 'E');
    [y_g, x_g] = find(map == 'G');
    
    num_elves = length(y_e);
    num_goblins = length(y_g);
    
    units(1:num_elves+num_goblins) = struct('y', 0, 'x', 0, 'type', ' ', 'hp', 200, 'power', 3);

    for i = 1:num_elves
        units(i) = struct('y', y_e(i), 'x', x_e(i), 'type', 'E', 'hp', 200, 'power', 3);
    end
    for i = 1:num_goblins
        units(num_elves + i) = struct('y', y_g(i), 'x', x_g(i), 'type', 'G', 'hp', 200, 'power', 3);
    end
end

function adj_enemies = find_adjacent_enemies(u_idx, units, enemy_indices)
    u = units(u_idx);
    adj_enemies = [];
    offsets = [-1 0; 0 -1; 0 1; 1 0];
    for i = 1:length(enemy_indices)
        e_idx = enemy_indices(i);
        e = units(e_idx);
        for j = 1:4
            if u.y + offsets(j,1) == e.y && u.x + offsets(j,2) == e.x
                adj_enemies = [adj_enemies, e_idx];
                break;
            end
        end
    end
end

function [map, units] = move_unit(u_idx, units, map, enemy_indices)
    [h, w] = size(map);
    offsets = [-1 0; 0 -1; 0 1; 1 0];
    
    target_squares = [];
    for i = 1:length(enemy_indices)
        e = units(enemy_indices(i));
        for j = 1:4
            ny = e.y + offsets(j,1);
            nx = e.x + offsets(j,2);
            if ny > 0 && ny <= h && nx > 0 && nx <= w && map(ny, nx) == '.'
                target_squares = [target_squares; ny, nx];
            end
        end
    end
    
    if isempty(target_squares)
        return;
    end
    
    dist = bfs(units(u_idx).y, units(u_idx).x, map);
    
    reachable_targets = [];
    min_dist = inf;
    for i = 1:size(target_squares, 1)
        ty = target_squares(i,1);
        tx = target_squares(i,2);
        d = dist(ty, tx);
        if d < min_dist
            min_dist = d;
            reachable_targets = [ty, tx];
        elseif d == min_dist
            reachable_targets = [reachable_targets; ty, tx];
        end
    end
    
    if isinf(min_dist)
        return;
    end
    
    reachable_targets = sortrows(reachable_targets, [1 2]);
    dest = reachable_targets(1,:);
    
    dist_to_dest = bfs(dest(1), dest(2), map);
    
    possible_steps = [];
    min_step_dist = inf;
    for j = 1:4
        ny = units(u_idx).y + offsets(j,1);
        nx = units(u_idx).x + offsets(j,2);
        if ny > 0 && ny <= h && nx > 0 && nx <= w && map(ny, nx) == '.'
            d = dist_to_dest(ny, nx);
            if d < min_step_dist
                min_step_dist = d;
                possible_steps = [ny, nx];
            elseif d == min_step_dist
                possible_steps = [possible_steps; ny, nx];
            end
        end
    end
    
    if isempty(possible_steps)
        return;
    end
    
    possible_steps = sortrows(possible_steps, [1 2]);
    next_step = possible_steps(1,:);
    
    map(units(u_idx).y, units(u_idx).x) = '.';
    map(next_step(1), next_step(2)) = units(u_idx).type;
    units(u_idx).y = next_step(1);
    units(u_idx).x = next_step(2);
end

function dist = bfs(start_y, start_x, map)
    [h, w] = size(map);
    dist = inf(h, w);
    q = [start_y, start_x];
    dist(start_y, start_x) = 0;
    head = 1;
    offsets = [-1 0; 0 -1; 0 1; 1 0];
    
    while head <= size(q, 1)
        curr = q(head,:);
        head = head + 1;
        
        for i = 1:4
            ny = curr(1) + offsets(i,1);
            nx = curr(2) + offsets(i,2);
            
            if ny > 0 && ny <= h && nx > 0 && nx <= w && map(ny, nx) == '.' && isinf(dist(ny, nx))
                dist(ny, nx) = dist(curr(1), curr(2)) + 1;
                q = [q; ny, nx];
            end
        end
    end
end
