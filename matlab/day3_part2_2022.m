
function main()
    fid = fopen('input.txt', 'r');
    if fid == -1
        error('Error opening file: input.txt');
    end
    
    sum = 0;
    groupItems = cell(1, 3);
    groupLineCounter = 0;
    
    tline = fgetl(fid);
    while ischar(tline)
        itemsMap = containers.Map('KeyType', 'char', 'ValueType', 'double');
        for i = 1:length(tline)
            item = tline(i);
            if isKey(itemsMap, item)
                itemsMap(item) = itemsMap(item) + 1;
            else
                itemsMap(item) = 1;
            end
        end
        groupItems{groupLineCounter + 1} = itemsMap;
        groupLineCounter = groupLineCounter + 1;
        
        if groupLineCounter == 3
            commonItems = containers.Map('KeyType', 'char', 'ValueType', 'double');
            keys1 = keys(groupItems{1});
            for k = 1:length(keys1)
                item = keys1{k};
                if isKey(groupItems{2}, item) && isKey(groupItems{3}, item)
                    commonItems(item) = 1;
                end
            end
            
            keysCommon = keys(commonItems);
            if ~isempty(keysCommon)
                sum = sum + itemPriority(keysCommon{1});
            end
            
            groupLineCounter = 0;
        end
        
        tline = fgetl(fid);
    end
    
    fclose(fid);
    
    fprintf('%d\n', sum);
end

function priority = itemPriority(item)
    if item >= 'a' && item <= 'z'
        priority = double(item) - double('a') + 1;
    else
        priority = double(item) - double('A') + 27;
    end
end

main();
