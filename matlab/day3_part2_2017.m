
function main()
    fileId = fopen('input.txt', 'r');
    target = str2double(fgetl(fileId));
    fclose(fileId);

    grid = containers.Map('KeyType', 'char', 'ValueType', 'double');
    grid('0,0') = 1;
    x = 0;
    y = 0;
    dx = 0;
    dy = -1;

    while true
        if x == y || (x < 0 && x == -y) || (x > 0 && x == 1 - y)
            temp_dx = dx;
            dx = -dy;
            dy = temp_dx;
        end

        x = x + dx;
        y = y + dy;

        value = 0;
        for i = -1:1
            for j = -1:1
                key = sprintf('%d,%d', x + i, y + j);
                if isKey(grid, key)
                    value = value + grid(key);
                end
            end
        end
        grid(sprintf('%d,%d', x, y)) = value;

        if value > target
            fprintf('%d\n', value);
            break;
        end
    end
end

main();
