
function main()
    rules = containers.Map('KeyType', 'char', 'ValueType', 'any');
    fileId = fopen('input.txt', 'r');
    
    while ~feof(fileId)
        line = strtrim(fgets(fileId));
        parts = strsplit(line, ' bags contain ');
        container = parts{1};
        contents = parts{2};
        
        if strcmp(contents, 'no other bags.')
            continue;
        end
        
        bagRules = {};
        matches = regexp(contents, '(\d+) (\w+ \w+) bags?[,.]', 'tokens');
        
        for i = 1:length(matches)
            count = str2double(matches{i}{1});
            color = matches{i}{2};
            bagRules{end+1} = struct('Color', color, 'Count', count);
        end
        
        rules(container) = bagRules;
    end
    
    fclose(fileId);
    
    totalBags = countBags('shiny gold', rules) - 1;
    disp(totalBags);
end

function count = countBags(color, rules)
    count = 1;
    if isKey(rules, color)
        bagRules = rules(color);
        for i = 1:length(bagRules)
            count = count + bagRules{i}.Count * countBags(bagRules{i}.Color, rules);
        end
    end
end

main();
