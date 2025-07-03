
function main()
    fileId = fopen('input.txt', 'r');
    directions = fscanf(fileId, '%c');
    fclose(fileId);

    houses = containers.Map('KeyType', 'char', 'ValueType', 'logical');
    x = 0;
    y = 0;
    houses(['0,0']) = true;

    for i = 1:length(directions)
        direction = directions(i);
        switch direction
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
        houses(key) = true;
    end

    disp(houses.Count);
end
