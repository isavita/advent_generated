
function main()
    fid = fopen('input.txt', 'r');
    if fid == -1
        error('Error opening file');
    end

    sensors = struct('pos', struct('x', {}, 'y', {}), ...
                     'beacon', struct('x', {}, 'y', {}), ...
                     'dist', {});
    sensorCount = 0;

    while ~feof(fid)
        line = fgetl(fid);
        if ischar(line)
            matches = sscanf(line, 'Sensor at x=%d, y=%d: closest beacon is at x=%d, y=%d');
            if ~isempty(matches)
                sensorCount = sensorCount + 1;
                sensors(sensorCount).pos.x = matches(1);
                sensors(sensorCount).pos.y = matches(2);
                sensors(sensorCount).beacon.x = matches(3);
                sensors(sensorCount).beacon.y = matches(4);
                sensors(sensorCount).dist = abs(sensors(sensorCount).pos.x - sensors(sensorCount).beacon.x) + abs(sensors(sensorCount).pos.y - sensors(sensorCount).beacon.y);
            end
        end
    end
    fclose(fid);

    targetY = 2000000;
    minX = Inf;
    maxX = -Inf;

    for i = 1:sensorCount
        distY = abs(sensors(i).pos.y - targetY);
        remainingDist = sensors(i).dist - distY;
        if remainingDist >= 0
            currentMinX = sensors(i).pos.x - remainingDist;
            currentMaxX = sensors(i).pos.x + remainingDist;
            minX = min(minX, currentMinX);
            maxX = max(maxX, currentMaxX);
        end
    end

    if isinf(minX) || isinf(maxX)
        impossibleCount = 0;
    else
        range = maxX - minX + 1;
        covered = false(1, range);

        for i = 1:sensorCount
            distY = abs(sensors(i).pos.y - targetY);
            remainingDist = sensors(i).dist - distY;
            if remainingDist >= 0
                startX = sensors(i).pos.x - remainingDist;
                endX = sensors(i).pos.x + remainingDist;
                
                startIdx = max(1, startX - minX + 1);
                endIdx = min(range, endX - minX + 1);
                
                covered(startIdx:endIdx) = true;
            end
        end

        for i = 1:sensorCount
            if sensors(i).beacon.y == targetY
                if sensors(i).beacon.x >= minX && sensors(i).beacon.x <= maxX
                    covered(sensors(i).beacon.x - minX + 1) = false;
                end
            end
        end
        impossibleCount = sum(covered);
    end

    fprintf('%d\n', impossibleCount);
end

main();
