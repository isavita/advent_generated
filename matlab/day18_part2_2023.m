
function solve()
    fid = fopen('input.txt', 'r');
    lines = textscan(fid, '%s', 'Delimiter', '\n');
    fclose(fid);
    input = lines{1};

    vertices = parseInput(input);
    result = calculatePolygonArea(vertices);
    fprintf('%d\n', result);
end

function vertices = parseInput(input)
    current = [0, 0];
    vertices = current;

    for i = 1:length(input)
        line = input{i};
        parts = strsplit(line, ' ');
        color = parts{3};
        dirInput = color(8);
        lengthStr = color(3:7);
        len = hex2dec(lengthStr);

        switch dirInput
            case '3' % Up
                dir = [0, -1];
            case '2' % Left
                dir = [-1, 0];
            case '1' % Down
                dir = [0, 1];
            case '0' % Right
                dir = [1, 0];
            otherwise
                error('Invalid direction input: %s', dirInput);
        end

        current = current + dir * len;
        vertices = [vertices; current];
    end
end

function area = shoelace(vertices)
    n = size(vertices, 1);
    area = 0;
    for i = 1:n
        next = mod(i, n) + 1;
        area = area + vertices(i, 1) * vertices(next, 2) - vertices(i, 2) * vertices(next, 1);
    end
    area = abs(area) / 2;
end

function perim = perimeter(vertices)
    n = size(vertices, 1);
    perim = 0;
    for i = 1:n
        next = mod(i, n) + 1;
        perim = perim + abs(vertices(i, 1) - vertices(next, 1)) + abs(vertices(i, 2) - vertices(next, 2));
    end
end

function result = calculatePolygonArea(vertices)
    result = shoelace(vertices) + perimeter(vertices) / 2 + 1;
end

solve();
