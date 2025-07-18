
function main()
    fid = fopen('input.txt', 'r');
    
    allIngredients = containers.Map('KeyType', 'char', 'ValueType', 'any');
    allergenCandidates = containers.Map('KeyType', 'char', 'ValueType', 'any');

    line = fgetl(fid);
    while ischar(line)
        parts = strsplit(line, ' (contains ');
        ingredients = strsplit(parts{1}, ' ');
        allergens = strsplit(strrep(parts{2}, ')', ''), ', ');

        for i = 1:numel(ingredients)
            ing = ingredients{i};
            if isKey(allIngredients, ing)
                allIngredients(ing) = allIngredients(ing) + 1;
            else
                allIngredients(ing) = 1;
            end
        end

        for i = 1:numel(allergens)
            alg = allergens{i};
            if isKey(allergenCandidates, alg)
                allergenCandidates(alg) = intersect(allergenCandidates(alg), ingredients);
            else
                allergenCandidates(alg) = ingredients;
            end
        end
        
        line = fgetl(fid);
    end
    fclose(fid);

    allCandidateIngredients = unique([allergenCandidates.values{:}]);
    allInputIngredients = keys(allIngredients);
    safeIngredients = setdiff(allInputIngredients, allCandidateIngredients);
    
    safeCount = 0;
    for i = 1:numel(safeIngredients)
        safeCount = safeCount + allIngredients(safeIngredients{i});
    end
    fprintf('%d\n', safeCount);

    dangerousIngredientsMap = containers.Map('KeyType', 'char', 'ValueType', 'char');
    while ~isempty(allergenCandidates)
        allergenKeys = keys(allergenCandidates);
        for i = 1:numel(allergenKeys)
            allergen = allergenKeys{i};
            candidates = allergenCandidates(allergen);
            if numel(candidates) == 1
                ingredient = candidates{1};
                dangerousIngredientsMap(allergen) = ingredient;
                remove(allergenCandidates, allergen);
                
                remainingAllergens = keys(allergenCandidates);
                for j = 1:numel(remainingAllergens)
                    remAllergen = remainingAllergens{j};
                    allergenCandidates(remAllergen) = setdiff(allergenCandidates(remAllergen), ingredient);
                end
                break;
            end
        end
    end

    sortedAllergens = sort(keys(dangerousIngredientsMap));
    dangerousList = values(dangerousIngredientsMap, sortedAllergens);
    fprintf('%s\n', strjoin(dangerousList, ','));
end
