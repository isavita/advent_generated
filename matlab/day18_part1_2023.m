
function main()
    file = fopen('input.txt', 'r');
    vertices = [0, 0];
    currentPos = [0, 0];

    while ~feof(file)
        line = fgetl(file);
        parts = strsplit(line);
        directionChar = parts{1};
        length = str2double(parts{2});

        switch directionChar
            case 'U'
                move = [0, 1];
            case 'D'
                move = [0, -1];
            case 'L'
                move = [-1, 0];
            case 'R'
                move = [1, 0];
        end

        currentPos = currentPos + move * length;
        vertices = [vertices; currentPos];
    end

    fclose(file);

    n = size(vertices, 1) - 1;
    area = 0;
    for i = 1:n
        area = area + vertices(i, 1) * vertices(i+1, 2) - vertices(i, 2) * vertices(i+1, 1);
    end
    area = abs(area) / 2;

    perimeter = 0;
    for i = 1:n
        perimeter = perimeter + abs(vertices(i, 1) - vertices(i+1, 1)) + abs(vertices(i, 2) - vertices(i+1, 2));
    end

    totalArea = area + perimeter / 2 + 1;
    fprintf('%d\n', totalArea);
end

main();
