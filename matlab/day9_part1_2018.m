
function main()
    [players, lastMarble] = readInput('input.txt');
    fprintf('%d\n', playMarbleGame(players, lastMarble));
end

function [players, lastMarble] = readInput(filename)
    fid = fopen(filename, 'r');
    line = fgetl(fid);
    fclose(fid);

    parts = strsplit(line);
    players = str2double(parts{1});
    lastMarble = str2double(parts{7});
end

function maxScore = playMarbleGame(players, lastMarble)
    scores = zeros(1, players);
    circle = 1;
    currentMarbleIndex = 1;
    circleSize = 1;

    for marble = 1:lastMarble
        if mod(marble, 23) == 0
            player = mod(marble, players);
            if player == 0
                player = players;
            end
            
            removeIndex = mod(currentMarbleIndex - 7 - 1, circleSize) + 1;
            scores(player) = scores(player) + marble + circle(removeIndex);
            
            circle(removeIndex) = [];
            circleSize = circleSize - 1;
            currentMarbleIndex = removeIndex;
            if currentMarbleIndex > circleSize
                currentMarbleIndex = 1;
            end
        else
            insertIndex = mod(currentMarbleIndex + 1, circleSize) + 1;
            circle = [circle(1:insertIndex-1), marble, circle(insertIndex:end)];
            circleSize = circleSize + 1;
            currentMarbleIndex = insertIndex;
        end
    end
    
    maxScore = max(scores);
end

main();
