
function main()
    fid = fopen('input.txt', 'r');
    if fid == -1
        error('Unable to open input.txt');
    end
    coordinates = textscan(fid, '%d, %d');
    fclose(fid);

    coordinates = [coordinates{1}, coordinates{2}];

    [minX, minY, maxX, maxY] = findBoundingBox(coordinates);
    maxDistance = 10000;
    regionSize = 0;

    for x = minX:maxX
        for y = minY:maxY
            totalDistance = sum(abs(x - coordinates(:,1)) + abs(y - coordinates(:,2)));
            if totalDistance < maxDistance
                regionSize = regionSize + 1;
            end
        end
    end

    fprintf('%d\n', regionSize);
end

function [minX, minY, maxX, maxY] = findBoundingBox(coordinates)
    minX = min(coordinates(:,1));
    minY = min(coordinates(:,2));
    maxX = max(coordinates(:,1));
    maxY = max(coordinates(:,2));
end

main();
