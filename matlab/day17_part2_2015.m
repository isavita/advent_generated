
function main()
    fid = fopen('input.txt', 'r');
    containers = textscan(fid, '%d');
    fclose(fid);
    containers = containers{1};

    minCount = inf;
    ways = 0;

    [minCount, ways] = findCombinations(containers, 150, 1, 0, minCount, ways);

    fprintf('%d\n', ways);
end

function [minCount, ways] = findCombinations(containers, target, index, count, minCount, ways)
    if target == 0
        if count < minCount
            minCount = count;
            ways = 1;
        elseif count == minCount
            ways = ways + 1;
        end
        return;
    end

    if target < 0 || index > length(containers)
        return;
    end

    [minCount, ways] = findCombinations(containers, target - containers(index), index + 1, count + 1, minCount, ways);
    [minCount, ways] = findCombinations(containers, target, index + 1, count, minCount, ways);
end

main();
