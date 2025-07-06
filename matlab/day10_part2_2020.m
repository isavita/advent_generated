
function main()
    fid = fopen('input.txt', 'r');
    adapters = [0];
    while ~feof(fid)
        line = strtrim(fgets(fid));
        if ~isempty(line)
            adapters = [adapters, str2double(line)];
        end
    end
    fclose(fid);

    adapters = sort(adapters);
    adapters = [adapters, adapters(end) + 3];

    disp(countArrangements(adapters));
end

function totalWays = countArrangements(adapters)
    ways = containers.Map('KeyType', 'double', 'ValueType', 'uint64');
    ways(0) = 1;

    for i = 2:length(adapters)
        currentJoltage = adapters(i);
        currentWays = 0;
        if isKey(ways, currentJoltage - 1)
            currentWays = currentWays + ways(currentJoltage - 1);
        end
        if isKey(ways, currentJoltage - 2)
            currentWays = currentWays + ways(currentJoltage - 2);
        end
        if isKey(ways, currentJoltage - 3)
            currentWays = currentWays + ways(currentJoltage - 3);
        end
        ways(currentJoltage) = currentWays;
    end
    totalWays = ways(adapters(end));
end

main();
