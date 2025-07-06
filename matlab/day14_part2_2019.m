
function main()
    reactions = containers.Map();
    ingredients = containers.Map();
    
    file = fopen('input.txt', 'r');
    while ~feof(file)
        line = strtrim(fgets(file));
        parts = strsplit(line, ' => ');
        
        outputParts = strsplit(parts{2});
        outputChem = outputParts{2};
        outputAmount = str2double(outputParts{1});
        reactions(outputChem) = struct('name', outputChem, 'amount', outputAmount);
        
        inputStrings = strsplit(parts{1}, ', ');
        currentIngredients = cell(1, length(inputStrings));
        for i = 1:length(inputStrings)
            inputParts = strsplit(inputStrings{i});
            currentIngredients{i} = struct('name', inputParts{2}, 'amount', str2double(inputParts{1}));
        end
        ingredients(outputChem) = currentIngredients;
    end
    fclose(file);
    
    oreAvailable = 1000000000000;
    disp(maxFuel(reactions, ingredients, oreAvailable));
end

function ore = calculateOre(chem, amount, reactions, ingredients, surplus)
    if strcmp(chem, 'ORE')
        ore = amount;
        return;
    end
    
    if isKey(surplus, chem) && surplus(chem) >= amount
        surplus(chem) = surplus(chem) - amount;
        ore = 0;
        return;
    end
    
    if isKey(surplus, chem)
        amount = amount - surplus(chem);
        surplus(chem) = 0;
    end
    
    reaction = reactions(chem);
    times = ceil(amount / reaction.amount);
    ore = 0;
    
    for i = 1:length(ingredients(chem))
        ingredient = ingredients(chem){i};
        ore = ore + calculateOre(ingredient.name, ingredient.amount * times, reactions, ingredients, surplus);
    end
    
    if isKey(surplus, chem)
        surplus(chem) = surplus(chem) + times * reaction.amount - amount;
    else
        surplus(chem) = times * reaction.amount - amount;
    end
end

function maxF = maxFuel(reactions, ingredients, oreAvailable)
    low = 0;
    high = oreAvailable;
    
    while low < high
        mid = floor((low + high + 1) / 2);
        surplus = containers.Map();
        oreNeeded = calculateOre('FUEL', mid, reactions, ingredients, surplus);
        
        if oreNeeded > oreAvailable
            high = mid - 1;
        else
            low = mid;
        end
    end
    maxF = low;
end

main();
