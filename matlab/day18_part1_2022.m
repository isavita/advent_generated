
function main()
    fileId = fopen("input.txt", "r");
    cubes = containers.Map('KeyType', 'char', 'ValueType', 'logical');
    while ~feof(fileId)
        line = strtrim(fgets(fileId));
        coords = strsplit(line, ",");
        x = str2double(coords{1});
        y = str2double(coords{2});
        z = str2double(coords{3});
        cubes(sprintf('%d,%d,%d', x, y, z)) = true;
    end
    fclose(fileId);

    surfaceArea = 0;
    keys = cubes.keys;
    for i = 1:length(keys)
        p = str2num(keys{i});
        exposedSides = 6;
        directions = [1, 0, 0; -1, 0, 0; 0, 1, 0; 0, -1, 0; 0, 0, 1; 0, 0, -1];
        for j = 1:size(directions, 1)
            adjacent = p + directions(j, :);
            if isKey(cubes, sprintf('%d,%d,%d', adjacent(1), adjacent(2), adjacent(3)))
                exposedSides = exposedSides - 1;
            end
        end
        surfaceArea = surfaceArea + exposedSides;
    end

    disp(surfaceArea);
end

main();
