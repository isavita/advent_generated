
function main()
    answer = solve_puzzle('input.txt');
    fprintf('%d\n', answer);
end

function steps = solve_puzzle(filename)
    initial_state = parse_input(filename);
    
    queue = {initial_state};
    visited = containers.Map('KeyType', 'char', 'ValueType', 'logical');
    
    initial_key = get_hash_key(initial_state);
    visited(initial_key) = true;
    
    while ~isempty(queue)
        current_state = queue{1};
        queue(1) = [];
        
        if is_done(current_state)
            steps = current_state.steps;
            return;
        end
        
        current_floor = current_state.elevator;
        
        gens_on_floor = find(current_state.floors(:, 1) == current_floor);
        chips_on_floor = find(current_state.floors(:, 2) == current_floor);
        
        item_indices = [gens_on_floor; -chips_on_floor];
        
        combos_1 = item_indices;
        combos_2 = [];
        if length(item_indices) >= 2
            combos_2 = nchoosek(item_indices, 2);
        end
        all_combos = [combos_1, zeros(size(combos_1,1), 1); combos_2];
        
        next_floors = [];
        if current_floor < 4, next_floors = [next_floors, current_floor + 1]; end
        if current_floor > 1, next_floors = [next_floors, current_floor - 1]; end
        
        for i = 1:size(all_combos, 1)
            items_to_move = all_combos(i, all_combos(i,:)~=0);
            for next_f = next_floors
                next_state = current_state;
                next_state.steps = current_state.steps + 1;
                next_state.elevator = next_f;
                
                for k = 1:length(items_to_move)
                    item = items_to_move(k);
                    if item > 0
                        next_state.floors(item, 1) = next_f;
                    else
                        next_state.floors(-item, 2) = next_f;
                    end
                end
                
                if is_valid(next_state)
                    key = get_hash_key(next_state);
                    if ~isKey(visited, key)
                        visited(key) = true;
                        queue{end+1} = next_state;
                    end
                end
            end
        end
    end
    steps = -1;
end

function state = parse_input(filename)
    fid = fopen(filename, 'r');
    lines = textscan(fid, '%s', 'Delimiter', '\n');
    lines = lines{1};
    fclose(fid);
    
    material_map = containers.Map();
    mat_id_counter = 0;
    floors = zeros(7, 2);
    
    for floor_num = 1:length(lines)
        parts = strsplit(lines{floor_num}, {' ', ',', '.'});
        for i = 1:length(parts)
            if strcmp(parts{i}, 'generator') || strcmp(parts{i}, 'microchip')
                mat_name = strrep(parts{i-1}, '-compatible', '');
                if ~isKey(material_map, mat_name)
                    mat_id_counter = mat_id_counter + 1;
                    material_map(mat_name) = mat_id_counter;
                end
                mat_id = material_map(mat_name);
                col = 1 + strcmp(parts{i}, 'microchip');
                floors(mat_id, col) = floor_num;
            end
        end
    end
    
    state.floors = floors(1:mat_id_counter, :);
    state.elevator = 1;
    state.steps = 0;
end

function key = get_hash_key(state)
    sorted_floors = sortrows(state.floors);
    key = sprintf('%d-%s', state.elevator, sprintf('%d', sorted_floors'));
end

function valid = is_valid(state)
    valid = true;
    for f = 1:4
        chips_on_floor = find(state.floors(:, 2) == f);
        gens_on_floor = find(state.floors(:, 1) == f);
        if ~isempty(gens_on_floor) && ~isempty(setdiff(chips_on_floor, gens_on_floor))
            valid = false;
            return;
        end
    end
end

function done = is_done(state)
    done = all(state.floors(:) == 4);
end
