
function main()
    distances = readAndParseInput('input.txt');
    locations = getUniqueLocations(distances);
    minDistance = findShortestRoute(locations, distances);
    disp(minDistance);
end

function distances = readAndParseInput(filename)
    fid = fopen(filename, 'r');
    lines = textscan(fid, '%s %s %s %s %d', 'Delimiter', ' ');
    fclose(fid);

    locations = unique([lines{1}; lines{3}]);
    numLocations = length(locations);
    distances = inf(numLocations, numLocations);
    locationMap = containers.Map(locations, 1:numLocations);

    for i = 1:length(lines{1})
        from = lines{1}{i};
        to = lines{3}{i};
        dist = lines{5}(i);

        fromIdx = locationMap(from);
        toIdx = locationMap(to);

        distances(fromIdx, toIdx) = dist;
        distances(toIdx, fromIdx) = dist;
    end
end

function locations = getUniqueLocations(distances)
    [rows, cols] = find(~isinf(distances));
    locations = unique([rows; cols]);
end

function minDistance = findShortestRoute(locations, distances)
    minDistance = inf;
    perms = perms(locations);
    for i = 1:size(perms, 1)
        currentRoute = perms(i, :);
        currentDistance = calculateRouteDistance(currentRoute, distances);
        minDistance = min(minDistance, currentDistance);
    end
end

function dist = calculateRouteDistance(route, distances)
    dist = 0;
    for i = 1:(length(route) - 1)
        dist = dist + distances(route(i), route(i+1));
    end
end

main();
