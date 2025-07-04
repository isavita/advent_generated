
function main()
    fileId = fopen('input.txt', 'r');
    lines = textscan(fileId, '%s', 'Delimiter', '\n');
    fclose(fileId);
    lines = lines{1};

    points = parseInput(lines);
    minVal = 2e14;
    maxVal = 4e14;
    count = solve(points, minVal, maxVal);
    disp(count);
end

function points = parseInput(lines)
    numLines = length(lines);
    points = struct('pos', cell(numLines, 1), 'vel', cell(numLines, 1));
    for i = 1:numLines
        line = lines{i};
        parts = strsplit(line, ' @ ');
        posParts = strsplit(parts{1}, ', ');
        velParts = strsplit(parts{2}, ', ');

        pos = zeros(1, 3);
        vel = zeros(1, 3);

        for j = 1:3
            pos(j) = str2double(posParts{j});
            vel(j) = str2double(velParts{j});
        end
        points(i).pos = pos;
        points(i).vel = vel;
    end
end

function count = solve(points, minVal, maxVal)
    numPoints = length(points);
    count = 0;
    for i = 1:numPoints
        for j = i+1:numPoints
            p1 = points(i);
            p2 = points(j);

            det = p1.vel(1) * p2.vel(2) - p2.vel(1) * p1.vel(2);

            if det ~= 0
                t1 = (p2.vel(2) * (p2.pos(1) - p1.pos(1)) - p2.vel(1) * (p2.pos(2) - p1.pos(2))) / det;
                t2 = (p1.vel(2) * (p2.pos(1) - p1.pos(1)) - p1.vel(1) * (p2.pos(2) - p1.pos(2))) / det;

                if t1 >= 0 && t2 >= 0
                    coordX = p1.pos(1) + p1.vel(1) * t1;
                    coordY = p1.pos(2) + p1.vel(2) * t1;

                    if coordX >= minVal && coordX <= maxVal && coordY >= minVal && coordY <= maxVal
                        count = count + 1;
                    end
                end
            end
        end
    end
end

main();
