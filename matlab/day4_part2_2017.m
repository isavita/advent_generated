
function main()
    fid = fopen('input.txt', 'r');
    if fid == -1
        error('File reading error');
    end
    passphrases = textscan(fid, '%s', 'Delimiter', '\n');
    fclose(fid);
    passphrases = strtrim(passphrases{1});

    validCount = 0;
    for i = 1:length(passphrases)
        words = strsplit(passphrases{i});
        sortedWords = cellfun(@(w) sortString(w), words, 'UniformOutput', false);
        if length(sortedWords) == length(unique(sortedWords))
            validCount = validCount + 1;
        end
    end
    disp(validCount);
end

function sortedStr = sortString(str)
    sortedStr = sort(str);
end
