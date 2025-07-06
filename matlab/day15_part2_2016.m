
function main()
    file = fopen('input.txt', 'r');
    discs = [];
    discRegex = regexp(fileread('input.txt'), 'Disc #\d+ has (\d+) positions; at time=0, it is at position (\d+).', 'tokens');

    for i = 1:length(discRegex)
        totalPositions = str2double(discRegex{i}{1});
        startPosition = str2double(discRegex{i}{2});
        discs(end+1, :) = [totalPositions, startPosition];
    end

    discs(end+1, :) = [11, 0];

    time = 0;
    while true
        if checkDiscs(discs, time)
            disp(time);
            break;
        end
        time = time + 1;
    end
end

function result = checkDiscs(discs, time)
    result = true;
    for i = 1:size(discs, 1)
        totalPositions = discs(i, 1);
        startPosition = discs(i, 2);
        position = mod(startPosition + time + i, totalPositions);
        if position ~= 0
            result = false;
            return;
        end
    end
end

main();
