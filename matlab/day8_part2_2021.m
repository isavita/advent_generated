
function main()
    fileId = fopen('input.txt', 'r');
    if fileId == -1
        error('Unable to open input.txt');
    end
    input = textscan(fileId, '%s', 'Delimiter', '\n');
    fclose(fileId);
    input = strtrim(strjoin(input{1}, '\n'));

    ans = jumbledSevenSegment(input);
    disp(ans);
end

function ans = jumbledSevenSegment(input)
    lines = strsplit(input, '\n');
    ans = 0;
    for i = 1:length(lines)
        line = lines{i};
        parts = regexp(line, '[a-g]+', 'match');
        if length(parts) ~= 14
            error('Expected 14 parts in line %d, got %d', i, length(parts));
        end

        alphabetizedParts = cell(size(parts));
        for j = 1:length(parts)
            alphabetizedParts{j} = sortString(parts{j});
        end

        workingSet = alphabetizedParts(1:10);
        outputSet = alphabetizedParts(11:14);

        indexToCharacters = cell(1, 10);
        killIndices = [];

        for j = 1:length(workingSet)
            mapping = workingSet{j};
            switch length(mapping)
                case 2
                    indexToCharacters{2} = mapping;
                    killIndices = [killIndices, j];
                case 4
                    indexToCharacters{5} = mapping;
                    killIndices = [killIndices, j];
                case 3
                    indexToCharacters{8} = mapping;
                    killIndices = [killIndices, j];
                case 7
                    indexToCharacters{9} = mapping;
                    killIndices = [killIndices, j];
            end
        end

        workingSet = removeSliceIndices(workingSet, killIndices);

        zeroThreeOrNine = {};
        killIndices = [];
        for j = 1:length(workingSet)
            mapping = workingSet{j};
            if checkStringOverlap(mapping, indexToCharacters{2})
                zeroThreeOrNine{end+1} = mapping;
                killIndices = [killIndices, j];
            end
        end

        if length(zeroThreeOrNine) ~= 3
            error('Expected 3 matches for 0, 3, or 9, got %d', length(zeroThreeOrNine));
        end

        newZeroThreeOrNine = {};
        for j = 1:length(zeroThreeOrNine)
            mapping = zeroThreeOrNine{j};
            if length(mapping) == 5
                indexToCharacters{4} = mapping;
            else
                newZeroThreeOrNine{end+1} = mapping;
            end
        end
        zeroThreeOrNine = newZeroThreeOrNine;

        newZeroThreeOrNine = {};
        for j = 1:length(zeroThreeOrNine)
            mapping = zeroThreeOrNine{j};
            if checkStringOverlap(mapping, indexToCharacters{5})
                indexToCharacters{10} = mapping;
            else
                newZeroThreeOrNine{end+1} = mapping;
            end
        end
        indexToCharacters{1} = newZeroThreeOrNine{1};

        workingSet = removeSliceIndices(workingSet, killIndices);
        if length(workingSet) ~= 3
            error('Expected length 3 at this stage, got %d', length(workingSet));
        end

        newWorkingSet = {};
        for j = 1:length(workingSet)
            mapping = workingSet{j};
            if length(mapping) == 6
                indexToCharacters{7} = mapping;
            else
                newWorkingSet{end+1} = mapping;
            end
        end
        workingSet = newWorkingSet;

        newWorkingSet = {};
        for j = 1:length(workingSet)
            mapping = workingSet{j};
            if checkStringOverlap(indexToCharacters{10}, mapping)
                indexToCharacters{6} = mapping;
            else
                newWorkingSet{end+1} = mapping;
            end
        end
        indexToCharacters{3} = newWorkingSet{1};

        num = 0;
        for j = 1:length(outputSet)
            out = outputSet{j};
            for k = 1:10
                if strcmp(out, indexToCharacters{k})
                    num = num * 10 + (k - 1);
                    break;
                end
            end
        end
        ans = ans + num;
    end
end

function sli = removeSliceIndices(sli, indices)
    sli(indices) = [];
end

function result = checkStringOverlap(larger, smaller)
    if length(larger) < length(smaller)
        temp = larger;
        larger = smaller;
        smaller = temp;
    end

    largeMap = containers.Map('KeyType', 'char', 'ValueType', 'logical');
    for k = 1:length(larger)
        largeMap(larger(k)) = true;
    end

    result = true;
    for k = 1:length(smaller)
        if ~isKey(largeMap, smaller(k))
            result = false;
            break;
        end
    end
end

function sortedString = sortString(input)
    chars = cellstr(input');
    sortedChars = sort(chars);
    sortedString = strjoin(sortedChars, '');
end

main();
