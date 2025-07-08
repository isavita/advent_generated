
% main.m
function main()
    ingredients = read_ingredients('input.txt');
    total_teaspoons = 100;
    target_calories = 500;
    
    capacities = [ingredients.capacity];
    durabilities = [ingredients.durability];
    flavors = [ingredients.flavor];
    textures = [ingredients.texture];
    calories_per_ing = [ingredients.calories];
    
    max_score = calculate_max_score_recursive(capacities, durabilities, flavors, textures, calories_per_ing, ...
                                              0, total_teaspoons, [], target_calories);
    
    fprintf('%d\n', max_score);
end

% read_ingredients.m
function ingredients = read_ingredients(filename)
    fid = fopen(filename, 'r');
    if fid == -1
        error('Could not open file %s', filename);
    end
    
    ingredients = struct('name', {}, 'capacity', {}, 'durability', {}, 'flavor', {}, 'texture', {}, 'calories', {});
    
    while ~feof(fid)
        tline = fgetl(fid);
        if ischar(tline)
            tokens = regexp(tline, '(\w+): capacity (-?\d+), durability (-?\d+), flavor (-?\d+), texture (-?\d+), calories (-?\d+)', 'tokens', 'once');
            
            if ~isempty(tokens)
                new_ingredient.name = tokens{1};
                new_ingredient.capacity = str2double(tokens{2});
                new_ingredient.durability = str2double(tokens{3});
                new_ingredient.flavor = str2double(tokens{4});
                new_ingredient.texture = str2double(tokens{5});
                new_ingredient.calories = str2double(tokens{6});
                
                ingredients(end+1) = new_ingredient;
            end
        end
    end
    fclose(fid);
end

% calculate_max_score_recursive.m
function max_score = calculate_max_score_recursive(capacities, durabilities, flavors, textures, calories_per_ing, ...
                                                     index, remaining, teaspoons_current_path, target_calories)
    num_ingredients = length(capacities);
    
    if index == num_ingredients - 1
        teaspoons_current_path = [teaspoons_current_path, remaining];
        
        current_calories = sum(calories_per_ing .* teaspoons_current_path);
        
        if current_calories == target_calories
            cap_sum = sum(capacities .* teaspoons_current_path);
            dur_sum = sum(durabilities .* teaspoons_current_path);
            fla_sum = sum(flavors .* teaspoons_current_path);
            tex_sum = sum(textures .* teaspoons_current_path);
            
            cap_sum = max(0, cap_sum);
            dur_sum = max(0, dur_sum);
            fla_sum = max(0, fla_sum);
            tex_sum = max(0, tex_sum);
            
            max_score = cap_sum * dur_sum * fla_sum * tex_sum;
        else
            max_score = 0;
        end
        return;
    end
    
    max_score = 0;
    for i = 0:remaining
        new_teaspoons_path = [teaspoons_current_path, i];
        current_score = calculate_max_score_recursive(capacities, durabilities, flavors, textures, calories_per_ing, ...
                                                      index + 1, remaining - i, new_teaspoons_path, target_calories);
        if current_score > max_score
            max_score = current_score;
        end
    end
end
