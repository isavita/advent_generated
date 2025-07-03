
function main()
    fileId = fopen('input.txt', 'r');
    lines = textscan(fileId, '%s', 'Delimiter', '\n');
    fclose(fileId);
    lines = lines{1};

    grid = containers.Map('KeyType', 'char', 'ValueType', 'logical');
    startX = 0;
    startY = 0;
    for y = 1:length(lines)
        line = lines{y};
        for x = 1:length(line)
            if line(x) == '#'
                grid([num2str(x-1), ',', num2str(y-1)]) = true;
            end
        end
        startX = floor(length(line) / 2);
        startY = floor((y-1) / 2);
    end

    dx = [0, 1, 0, -1];
    dy = [-1, 0, 1, 0];

    x = startX;
    y = startY;
    dir = 0;
    infectedCount = 0;

    for i = 1:10000
        posKey = [num2str(x), ',', num2str(y)];
        if isKey(grid, posKey)
            dir = mod(dir + 1, 4);
            remove(grid, posKey);
        else
            dir = mod(dir - 1 + 4, 4);
            grid(posKey) = true;
            infectedCount = infectedCount + 1;
        end
        x = x + dx(dir + 1);
        y = y + dy(dir + 1);
    end

    disp(infectedCount);
end

main();
