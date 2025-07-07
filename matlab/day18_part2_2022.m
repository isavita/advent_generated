
function main()
    fid = fopen('input.txt', 'r');
    lines = textscan(fid, '%s', 'Delimiter', '\n');
    fclose(fid);
    lines = lines{1};

    cubes = containers.Map('KeyType', 'char', 'ValueType', 'logical');
    minCoords = [Inf, Inf, Inf];
    maxCoords = [-Inf, -Inf, -Inf];

    for i = 1:length(lines)
        if isempty(lines{i})
            continue;
        end
        parts = strsplit(lines{i}, ',');
        x = str2double(parts{1});
        y = str2double(parts{2});
        z = str2double(parts{3});
        cubes([num2str(x), ',', num2str(y), ',', num2str(z)]) = true;
        minCoords = min(minCoords, [x, y, z]);
        maxCoords = max(maxCoords, [x, y, z]);
    end

    minCoords = minCoords - 1;
    maxCoords = maxCoords + 1;

    neighbors = [
        -1, 0, 0;
        1, 0, 0;
        0, -1, 0;
        0, 1, 0;
        0, 0, -1;
        0, 0, 1
    ];

    faces = 0;
    q = {minCoords};
    seen = containers.Map('KeyType', 'char', 'ValueType', 'logical');
    seen([num2str(minCoords(1)), ',', num2str(minCoords(2)), ',', num2str(minCoords(3))]) = true;

    while ~isempty(q)
        curr = q{1};
        q(1) = [];

        for i = 1:size(neighbors, 1)
            next = curr + neighbors(i, :);

            if any(next < minCoords) || any(next > maxCoords)
                continue;
            end

            nextKey = [num2str(next(1)), ',', num2str(next(2)), ',', num2str(next(3))];

            if isKey(cubes, nextKey)
                faces = faces + 1;
            elseif ~isKey(seen, nextKey)
                seen(nextKey) = true;
                q{end+1} = next;
            end
        end
    end

    disp(faces);
end

main();
