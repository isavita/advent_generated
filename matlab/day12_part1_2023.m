
function main()
    input = readFile("input.txt");
    rows = parseInput(input);
    totalArrangements = 0;
    for i = 1:length(rows)
        totalArrangements = totalArrangements + countArrangements(rows(i));
    end
    disp(totalArrangements);
end

function rows = parseInput(input)
    rows = struct('Springs', {}, 'Group', {});
    for i = 1:length(input)
        parts = strsplit(input{i}, ' ');
        springs = parts{1};
        groups = str2double(strsplit(parts{2}, ','));
        rows(i).Springs = springs;
        rows(i).Group = groups;
    end
end

function count = countArrangements(row)
    cache = containers.Map('KeyType', 'char', 'ValueType', 'double');
    count = countArrangementsRecursive(row, 1, 1, 0, cache);
end

function count = countArrangementsRecursive(row, iSprings, iGroup, iContiguousDamaged, cache)
    cacheKey = sprintf('%d-%d-%d', iSprings, iGroup, iContiguousDamaged);
    if isKey(cache, cacheKey)
        count = cache(cacheKey);
        return;
    end

    if iSprings > length(row.Springs)
        if iGroup > length(row.Group) && iContiguousDamaged == 0
            count = 1;
        elseif iGroup == length(row.Group) && iContiguousDamaged == row.Group(iGroup)
            count = 1;
        else
            count = 0;
        end
        return;
    end

    count = 0;
    char = row.Springs(iSprings);

    if char == '.' || char == '?'
        if iContiguousDamaged == 0
            count = count + countArrangementsRecursive(row, iSprings + 1, iGroup, iContiguousDamaged, cache);
        elseif iGroup <= length(row.Group) && iContiguousDamaged == row.Group(iGroup)
            count = count + countArrangementsRecursive(row, iSprings + 1, iGroup + 1, 0, cache);
        end
    end

    if char == '#' || char == '?'
        if iGroup <= length(row.Group) && iContiguousDamaged < row.Group(iGroup)
            count = count + countArrangementsRecursive(row, iSprings + 1, iGroup, iContiguousDamaged + 1, cache);
        end
    end

    cache(cacheKey) = count;
end

function inputLines = readFile(fileName)
    fid = fopen(fileName, 'r');
    if fid == -1
        error('Cannot open file: %s', fileName);
    end
    inputLines = textscan(fid, '%s', 'Delimiter', '\n');
    fclose(fid);
    inputLines = inputLines{1};
end

main();
