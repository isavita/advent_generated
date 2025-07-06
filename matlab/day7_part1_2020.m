
function main()
    fid = fopen('input.txt', 'r');
    if fid == -1
        error('Error opening file: input.txt');
    end
    
    contains = containers.Map();
    
    tline = fgetl(fid);
    while ischar(tline)
        parts = strsplit(tline, ' bags contain ');
        container = parts{1};
        
        if strcmp(parts{2}, 'no other bags.')
            tline = fgetl(fid);
            continue;
        end
        
        containedBags = strsplit(parts{2}, ', ');
        for i = 1:length(containedBags)
            bagInfo = strsplit(containedBags{i});
            bagName = strjoin(bagInfo(2:3), ' ');
            
            if contains.isKey(bagName)
                contains(bagName) = [contains(bagName), {container}];
            else
                contains(bagName) = {container};
            end
        end
        tline = fgetl(fid);
    end
    
    fclose(fid);
    
    count = countCanContain('shiny gold', contains);
    disp(count);
end

function count = countCanContain(target, contains)
    seen = containers.Map();
    
    function dfs(bag)
        if contains.isKey(bag)
            outerBags = contains(bag);
            for i = 1:length(outerBags)
                outerBag = outerBags{i};
                if ~seen.isKey(outerBag)
                    seen(outerBag) = true;
                    dfs(outerBag);
                end
            end
        end
    end
    
    dfs(target);
    count = seen.Count;
end

main();
