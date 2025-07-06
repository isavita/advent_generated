
function main()
    fid = fopen('input.txt', 'r');
    if fid == -1
        error('Error opening file: input.txt');
    end
    
    totalCount = 0;
    groupAnswers = containers.Map('KeyType', 'char', 'ValueType', 'double');
    groupSize = 0;
    
    tline = fgetl(fid);
    while ischar(tline)
        if isempty(tline)
            for key = groupAnswers.keys
                if groupAnswers(key{1}) == groupSize
                    totalCount = totalCount + 1;
                end
            end
            groupAnswers = containers.Map('KeyType', 'char', 'ValueType', 'double');
            groupSize = 0;
        else
            groupSize = groupSize + 1;
            for i = 1:length(tline)
                char_tline = tline(i);
                if isKey(groupAnswers, char_tline)
                    groupAnswers(char_tline) = groupAnswers(char_tline) + 1;
                else
                    groupAnswers(char_tline) = 1;
                end
            end
        end
        tline = fgetl(fid);
    end
    
    for key = groupAnswers.keys
        if groupAnswers(key{1}) == groupSize
            totalCount = totalCount + 1;
        end
    end
    
    fclose(fid);
    disp(totalCount);
end

main();
