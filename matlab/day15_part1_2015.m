
function main()
    ingredients = readIngredients('input.txt');
    totalTeaspoons = 100;
    
    numIngredients = numel(ingredients);
    initialTeaspoons = zeros(1, numIngredients); 
    
    maxScore = calculateMaxScore(ingredients, 1, totalTeaspoons, initialTeaspoons);
    
    fprintf('%d\n', maxScore);
end

main();

function ingredients = readIngredients(filename)
    fid = fopen(filename, 'r');
    if fid == -1
        error('Could not open file: %s', filename);
    end
    
    ingredients = struct([]); 
    k = 0;
    
    while ~feof(fid)
        line = fgetl(fid);
        if ~ischar(line), break; end 
        
        parts = strsplit(line, ' ');
        
        if numel(parts) < 11
            continue; 
        end

        k = k + 1;
        ingredients(k).name = strrep(parts{1}, ':', '');
        ingredients(k).capacity = str2double(strrep(parts{3}, ',', ''));
        ingredients(k).durability = str2double(strrep(parts{5}, ',', ''));
        ingredients(k).flavor = str2double(strrep(parts{7}, ',', ''));
        ingredients(k).texture = str2double(strrep(parts{9}, ',', ''));
    end
    
    fclose(fid);
end

function maxScore = calculateMaxScore(ingredients, index, remainingTeaspoons, currentTeaspoons)
    numIngredients = numel(ingredients);

    if index == numIngredients
        currentTeaspoons(index) = remainingTeaspoons;
        maxScore = score(ingredients, currentTeaspoons);
        return;
    end

    maxScore = 0;
    for i = 0:remainingTeaspoons
        currentTeaspoons(index) = i; 
        scoreVal = calculateMaxScore(ingredients, index + 1, remainingTeaspoons - i, currentTeaspoons);
        maxScore = max(maxScore, scoreVal);
    end
end

function totalScore = score(ingredients, teaspoons)
    capacity = 0;
    durability = 0;
    flavor = 0;
    texture = 0;
    
    numIngredients = numel(ingredients);
    for i = 1:numIngredients
        capacity = capacity + ingredients(i).capacity * teaspoons(i);
        durability = durability + ingredients(i).durability * teaspoons(i);
        flavor = flavor + ingredients(i).flavor * teaspoons(i);
        texture = texture + ingredients(i).texture * teaspoons(i);
    end
    
    capacity = max(0, capacity);
    durability = max(0, durability);
    flavor = max(0, flavor);
    texture = max(0, texture);
    
    totalScore = capacity * durability * flavor * texture;
end
