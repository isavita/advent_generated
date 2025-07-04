
function main()
    fid = fopen('input.txt', 'r');
    times = [];
    distances = [];
    
    line = fgetl(fid);
    values = strsplit(line);
    for i = 1:length(values)
        if ~isempty(str2num(values{i}))
            times = [times, str2num(values{i})];
        end
    end
    
    line = fgetl(fid);
    values = strsplit(line);
    for i = 1:length(values)
        if ~isempty(str2num(values{i}))
            distances = [distances, str2num(values{i})];
        end
    end
    
    fclose(fid);
    
    totalWays = 1;
    for i = 1:length(times)
        totalWays = totalWays * numberOfWaysToWin(times(i), distances(i));
    end
    
    fprintf('Total ways to win: %d\n', totalWays);
end

function ways = numberOfWaysToWin(time, record)
    ways = 0;
    for holdTime = 1:time
        travelTime = time - holdTime;
        distance = holdTime * travelTime;
        if distance > record
            ways = ways + 1;
        end
    end
end

main();
