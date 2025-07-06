
function main()
    fid = fopen('input.txt', 'r');
    if fid == -1
        error('Unable to open input.txt');
    end
    messages = textscan(fid, '%s');
    fclose(fid);
    
    messages = messages{1};
    
    if isempty(messages)
        disp('');
        return;
    end
    
    messageLength = length(messages{1});
    counts = cell(1, messageLength);
    for i = 1:messageLength
        counts{i} = containers.Map('KeyType', 'char', 'ValueType', 'double');
    end
    
    for i = 1:length(messages)
        for j = 1:messageLength
            char = messages{i}(j);
            if isKey(counts{j}, char)
                counts{j}(char) = counts{j}(char) + 1;
            else
                counts{j}(char) = 1;
            end
        end
    end
    
    correctedMessage = '';
    for i = 1:messageLength
        correctedMessage = [correctedMessage, getMostCommonChar(counts{i})];
    end
    
    disp(correctedMessage);
end

function mostCommonChar = getMostCommonChar(countMap)
    keys = countMap.keys;
    values = countMap.values;
    [~, maxIndex] = max([values{:}]);
    mostCommonChar = keys{maxIndex};
end

main();
