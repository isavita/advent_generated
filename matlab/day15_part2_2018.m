
function main
    elf_power = 3;
    final_outcome = 0;
    while true
        elf_power = elf_power + 1;
        [map, units, rows, cols, num_units] = parse_map(elf_power);
        rounds = 0;
        elf_died = false;
        while true
            [combat_continues, hp_sum] = get_status(units, num_units);
            if ~combat_continues
                final_outcome = rounds * hp_sum;
                break
            end
            [round_completed, elf_died, map, units, num_units] = tick(map, units, rows, cols, num_units, true);
            if elf_died
                break
            end
            if ~round_completed && ~elf_died
                [~ , hp_sum] = get_status(units, num_units);
                final_outcome = rounds * hp_sum;
                break
            end
            rounds = rounds + 1;
        end
        if ~elf_died
            break
        end
    end
    fprintf('%d\n', final_outcome);
end

function [map, units, rows, cols, num_units] = parse_map(elf_power)
    fid = fopen('input.txt');
    lines = cellfun(@strtrim, textread('input.txt', '%s', 'whitespace', ''), 'UniformOutput', false);
    fclose(fid);
    rows = numel(lines);
    cols = max(cellfun(@length, lines));
    map = repmat(struct('kind', 1, 'x', 0, 'y', 0), rows, cols);
    units = repmat(struct('id', 0, 'kind', 1, 'hp', 200, 'power', 3, 'x', 0, 'y', 0, 'alive', true), 50, 1);
    num_units = 0;
    id = 0;
    for y = 1:rows
        line = lines{y};
        for x = 1:cols
            ch = line(x);
            switch ch
                case '#'
                    map(y, x).kind = 8;
                case '.'
                    map(y, x).kind = 1;
                case 'E'
                    map(y, x).kind = 1;
                    id = id + 1;
                    num_units = num_units + 1;
                    units(num_units) = struct('id', id, 'kind', 2, 'hp', 200, 'power', elf_power, 'x', x, 'y', y, 'alive', true);
                case 'G'
                    map(y, x).kind = 1;
                    id = id + 1;
                    num_units = num_units + 1;
                    units(num_units) = struct('id', id, 'kind', 4, 'hp', 200, 'power', 3, 'x', x, 'y', y, 'alive', true);
            end
            map(y, x).x = x;
            map(y, x).y = y;
        end
    end
    units = units(1:num_units);
end

function [combat_continues, hp_sum] = get_status(units, num_units)
    elves_exist = false;
    goblins_exist = false;
    hp_sum = 0;
    for i = 1:num_units
        if units(i).alive
            if units(i).kind == 2
                elves_exist = true;
            else
                goblins_exist = true;
            end
            hp_sum = hp_sum + units(i).hp;
        end
    end
    combat_continues = elves_exist && goblins_exist;
end

function [round_completed, elf_died, map, units, num_units] = tick(map, units, rows, cols, num_units, stop_on_elf_death)
    units = sort_units(units, num_units);
    elf_died = false;
    for i = 1:num_units
        if ~units(i).alive
            continue
        end
        current_unit = units(i);
        target_kind = 4;
        if current_unit.kind == 4
            target_kind = 2;
        end
        targets_exist = false;
        for j = 1:num_units
            if units(j).alive && units(j).kind == target_kind
                targets_exist = true;
                break
            end
        end
        if ~targets_exist
            round_completed = false;
            return
        end
        attack_target = [];
        min_hp = inf;
        offsets = [0 -1; -1 0; 1 0; 0 1];
        for k = 1:4
            nx = current_unit.x + offsets(k, 1);
            ny = current_unit.y + offsets(k, 2);
            if nx >= 1 && nx <= cols && ny >= 1 && ny <= rows
                for j = 1:num_units
                    if units(j).alive && units(j).kind == target_kind && units(j).x == nx && units(j).y == ny
                        if units(j).hp < min_hp
                            min_hp = units(j).hp;
                            attack_target = j;
                        end
                        break
                    end
                end
            end
        end
        if isempty(attack_target)
            [dist, path] = bfs(map, rows, cols, current_unit.x, current_unit.y);
            targets = [];
            target_count = 0;
            min_dist = inf;
            for j = 1:num_units
                if units(j).alive && units(j).kind == target_kind
                    for k = 1:4
                        nx = units(j).x + offsets(k, 1);
                        ny = units(j).y + offsets(k, 2);
                        if nx >= 1 && nx <= cols && ny >= 1 && ny <= rows && map(ny, nx).kind == 1 && dist(ny, nx) ~= -1
                            if dist(ny, nx) < min_dist
                                min_dist = dist(ny, nx);
                                target_count = 1;
                                targets = [nx ny];
                            elseif dist(ny, nx) == min_dist
                                target_count = target_count + 1;
                                targets = [targets; nx ny];
                            end
                        end
                    end
                end
            end
            if target_count > 0
                [~, idx] = sortrows(targets, [2 1]);
                chosen_target = targets(idx(1), :);
                current_step = chosen_target;
                while dist(current_step(2), current_step(1)) > 1
                    current_step = path(current_step(2), current_step(1), :);
                    if current_step(1) == -1
                        break
                    end
                end
                if current_step(1) ~= -1 && dist(current_step(2), current_step(1)) == 1
                    map(current_unit.y, current_unit.x).kind = 1;
                    units(i).x = current_step(1);
                    units(i).y = current_step(2);
                    map(units(i).y, units(i).x).kind = units(i).kind;
                    min_hp = inf;
                    attack_target = [];
                    for k = 1:4
                        nx = units(i).x + offsets(k, 1);
                        ny = units(i).y + offsets(k, 2);
                        if nx >= 1 && nx <= cols && ny >= 1 && ny <= rows
                            for j = 1:num_units
                                if units(j).alive && units(j).kind == target_kind && units(j).x == nx && units(j).y == ny
                                    if units(j).hp < min_hp
                                        min_hp = units(j).hp;
                                        attack_target = j;
                                    end
                                    break
                                end
                            end
                        end
                    end
                end
            end
        end
        if ~isempty(attack_target)
            units(attack_target).hp = units(attack_target).hp - units(i).power;
            if units(attack_target).hp <= 0
                units(attack_target).alive = false;
                map(units(attack_target).y, units(attack_target).x).kind = 1;
                if units(attack_target).kind == 2 && stop_on_elf_death
                    elf_died = true;
                    round_completed = true;
                    return
                end
            end
        end
    end
    live_units = 0;
    for k = 1:num_units
        if units(k).alive
            live_units = live_units + 1;
            units(live_units) = units(k);
        end
    end
    num_units = live_units;
    units = units(1:num_units);
    round_completed = true;
end

function [dist, path] = bfs(map, rows, cols, sx, sy)
    dist = -ones(rows, cols);
    path = repmat([-1 -1], rows, cols, 2);
    queue = [sx sy];
    dist(sy, sx) = 0;
    q_head = 1;
    offsets = [0 -1; -1 0; 1 0; 0 1];
    while q_head <= size(queue, 1)
        curr = queue(q_head, :);
        q_head = q_head + 1;
        for i = 1:4
            next_x = curr(1) + offsets(i, 1);
            next_y = curr(2) + offsets(i, 2);
            if next_x >= 1 && next_x <= cols && next_y >= 1 && next_y <= rows && map(next_y, next_x).kind == 1 && dist(next_y, next_x) == -1
                dist(next_y, next_x) = dist(curr(2), curr(1)) + 1;
                path(next_y, next_x, :) = [curr(1) curr(2)];
                queue = [queue; next_x next_y];
            end
        end
    end
end

function units = sort_units(units, num_units)
    if num_units <= 1
        return
    end
    [~, idx] = sortrows([units.y; units.x]');
    units = units(idx);
end
