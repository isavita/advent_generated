
function main
    points = readInput('input.txt');
    [second, messagePoints] = simulate(points);
    printMessage(messagePoints, second);
end

function points = readInput(filePath)
    fid = fopen(filePath, 'r');
    lines = textscan(fid, '%s', 'Delimiter', '\n');
    fclose(fid);
    lines = lines{1};

    points = zeros(length(lines), 4);
    for i = 1:length(lines)
        tokens = regexp(lines{i}, '-?\d+', 'match');
        points(i, :) = cellfun(@str2double, tokens);
    end
end

function [seconds, messagePoints] = simulate(points)
    seconds = 0;
    prevArea = inf;
    prevPoints = [];

    while true
        positions = points(:, 1:2) + points(:, 3:4);
        points(:, 1:2) = positions;

        minX = min(positions(:, 1));
        maxX = max(positions(:, 1));
        minY = min(positions(:, 2));
        maxY = max(positions(:, 2));

        area = (maxX - minX) * (maxY - minY);

        if seconds > 0 && area > prevArea
            messagePoints = prevPoints;
            return;
        end

        prevArea = area;
        prevPoints = positions;
        seconds = seconds + 1;
    end
end

function printMessage(points, second)
    minX = min(points(:, 1));
    maxX = max(points(:, 1));
    minY = min(points(:, 2));
    maxY = max(points(:, 2));

    sky = repmat('.', maxY - minY + 1, maxX - minX + 1);
    for i = 1:size(points, 1)
        sky(points(i, 2) - minY + 1, points(i, 1) - minX + 1) = '#';
    end

    fprintf('After %d seconds:\n', second);
    for i = 1:size(sky, 1)
        fprintf('%s\n', sky(i, :));
    end
end
