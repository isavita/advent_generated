
function main()
    fid = fopen('input.txt', 'r');
    
    line = fgetl(fid);
    initial_state_str = sscanf(line, 'initial state: %s');
    
    fgetl(fid);
    
    rules = containers.Map('KeyType', 'char', 'ValueType', 'logical');
    while ~feof(fid)
        line = fgetl(fid);
        if isempty(line)
            continue;
        end
        parts = sscanf(line, '%s => %c');
        if parts(end) == '#'
            rules(parts(1:5)) = true;
        end
    end
    fclose(fid);

    plant_indices = find(initial_state_str == '#') - 1;

    total_generations = 50000000000;
    previous_pattern = [];
    previous_sum = 0;

    for gen = 1:200
        min_idx = min(plant_indices);
        max_idx = max(plant_indices);
        
        new_plant_indices = [];
        
        for i = (min_idx - 2):(max_idx + 2)
            pattern_indices = (i-2):(i+2);
            has_plant = ismember(pattern_indices, plant_indices);
            pattern_str = char(has_plant * ('#' - '.') + '.');
            
            if isKey(rules, pattern_str)
                new_plant_indices(end+1) = i;
            end
        end
        
        plant_indices = new_plant_indices;
        
        current_sum = sum(plant_indices);
        current_pattern = plant_indices - min(plant_indices);
        
        if isequal(current_pattern, previous_pattern)
            offset = current_sum - previous_sum;
            remaining_generations = total_generations - gen;
            final_sum = current_sum + offset * remaining_generations;
            
            fprintf('%.0f\n', final_sum);
            return;
        end
        
        previous_pattern = current_pattern;
        previous_sum = current_sum;
    end
end
