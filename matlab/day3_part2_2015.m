
function main()
    fid = fopen('input.txt', 'r');
    directions = fscanf(fid, '%c');
    fclose(fid);

    houses = containers.Map('KeyType', 'char', 'ValueType', 'double');
    x = 0;
    y = 0;
    rx = 0;
    ry = 0;

    key = sprintf('%d,%d', x, y);
    houses(key) = 2;

    for i = 1:length(directions)
        if mod(i, 2) == 1
            switch directions(i)
                case '^'
                    y = y + 1;
                case 'v'
                    y = y - 1;
                case '>'
                    x = x + 1;
                case '<'
                    x = x - 1;
            end
            key = sprintf('%d,%d', x, y);
            if isKey(houses, key)
                houses(key) = houses(key) + 1;
            else
                houses(key) = 1;
            end
        else
            switch directions(i)
                case '^'
                    ry = ry + 1;
                case 'v'
                    ry = ry - 1;
                case '>'
                    rx = rx + 1;
                case '<'
                    rx = rx - 1;
            end
            key = sprintf('%d,%d', rx, ry);
            if isKey(houses, key)
                houses(key) = houses(key) + 1;
            else
                houses(key) = 1;
            end
        end
    end

    answer = length(houses);
    disp(answer);
end

main();
