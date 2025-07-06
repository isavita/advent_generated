
function main()
    fid = fopen('input.txt', 'r');
    lines = textscan(fid, '%s', 'Delimiter', '\n');
    fclose(fid);
    lines = lines{1};

    stars = [];
    for i = 1:length(lines)
        line = lines{i};
        match = regexp(line, 'position=<\s*(-?\d+),\s*(-?\d+)> velocity=<\s*(-?\d+),\s*(-?\d+)>', 'tokens');
        if ~isempty(match)
            x = str2double(match{1}{1});
            y = str2double(match{1}{2});
            vx = str2double(match{1}{3});
            vy = str2double(match{1}{4});
            stars = [stars; struct('x', x, 'y', y, 'vx', vx, 'vy', vy)];
        end
    end

    smallestT = 0;
    smallestArea = inf;

    for t = 1:100000
        currentX = [stars.x] + [stars.vx] * t;
        currentY = [stars.y] + [stars.vy] * t;

        minX = min(currentX);
        maxX = max(currentX);
        minY = min(currentY);
        maxY = max(currentY);

        area = (maxX - minX + 1) + (maxY - minY + 1);

        if area < smallestArea
            smallestArea = area;
            smallestT = t;
        end
    end

    fprintf('%d\n', smallestT);

    t = smallestT;
    finalX = [stars.x] + [stars.vx] * t;
    finalY = [stars.y] + [stars.vy] * t;

    minX = min(finalX);
    maxX = max(finalX);
    minY = min(finalY);
    maxY = max(finalY);

    image = false(maxY - minY + 1, maxX - minX + 1);

    for i = 1:length(stars)
        image(finalY(i) - minY + 1, finalX(i) - minX + 1) = true;
    end

    for row = 1:size(image, 1)
        for col = 1:size(image, 2)
            if image(row, col)
                fprintf('#');
            else
                fprintf(' ');
            end
        end
        fprintf('\n');
    end
end

main();
