
function main()
    fid = fopen('input.txt', 'r');
    data = textscan(fid, '%s', 'Delimiter', '\n');
    fclose(fid);
    data = data{1};

    allergens = containers.Map('KeyType', 'char', 'ValueType', 'any');
    ingredients_count = containers.Map('KeyType', 'char', 'ValueType', 'double');

    for i = 1:length(data)
        line = data{i};
        parts = strsplit(line, ' (contains ');
        ingredients_str = parts{1};
        allergens_str = strrep(parts{2}, ')', '');
        
        ingredients = strsplit(ingredients_str);
        allergens_list = strsplit(allergens_str, ', ');
        
        for j = 1:length(ingredients)
            ingredient = ingredients{j};
            if ~isKey(ingredients_count, ingredient)
                ingredients_count(ingredient) = 0;
            end
            ingredients_count(ingredient) = ingredients_count(ingredient) + 1;
        end
        
        for j = 1:length(allergens_list)
            allergen = allergens_list{j};
            current_ingredients_set = containers.Map('KeyType', 'char', 'ValueType', 'logical');
            for k = 1:length(ingredients)
                current_ingredients_set(ingredients{k}) = true;
            end

            if ~isKey(allergens, allergen)
                allergens(allergen) = current_ingredients_set;
            else
                existing_ingredients = allergens(allergen);
                new_allergens = containers.Map('KeyType', 'char', 'ValueType', 'logical');
                keys_existing = keys(existing_ingredients);
                for k = 1:length(keys_existing)
                    key = keys_existing{k};
                    if isKey(current_ingredients_set, key)
                        new_allergens(key) = true;
                    end
                end
                allergens(allergen) = new_allergens;
            end
        end
    end

    allergen_ingredients_map = containers.Map('KeyType', 'char', 'ValueType', 'logical');
    keys_allergens = keys(allergens);
    for i = 1:length(keys_allergens)
        allergen = keys_allergens{i};
        ingredients_for_allergen = allergens(allergen);
        keys_ingredients = keys(ingredients_for_allergen);
        for j = 1:length(keys_ingredients)
            allergen_ingredients_map(keys_ingredients{j}) = true;
        end
    end

    safe_ingredients_count = 0;
    keys_ingredients_count = keys(ingredients_count);
    for i = 1:length(keys_ingredients_count)
        ingredient = keys_ingredients_count{i};
        if ~isKey(allergen_ingredients_map, ingredient)
            safe_ingredients_count = safe_ingredients_count + ingredients_count(ingredient);
        end
    end

    disp(safe_ingredients_count);
end

main();
