
function main()
    fid = fopen('input.txt', 'r');
    points = [];
    folds = [];
    readingPoints = true;

    while ~feof(fid)
        line = strtrim(fgetl(fid));
        if isempty(line)
            readingPoints = false;
            continue;
        end

        if readingPoints
            coords = sscanf(line, '%d,%d');
            points = [points; coords'];
        else
            if startsWith(line, 'fold along x=')
                val = sscanf(line, 'fold along x=%d');
                folds = [folds; [val, 0]];
            elseif startsWith(line, 'fold along y=')
                val = sscanf(line, 'fold along y=%d');
                folds = [folds; [0, val]];
            end
        end
    end
    fclose(fid);

    for i = 1:size(folds, 1)
        fold = folds(i, :);
        if fold(1) ~= 0
            points(:, 1) = fold(1) - abs(points(:, 1) - fold(1));
        elseif fold(2) ~= 0
            points(:, 2) = fold(2) - abs(points(:, 2) - fold(2));
        end
        points = unique(points, 'rows');

        if i == 1
            fprintf('Number of dots visible after first fold: %d\n', size(points, 1));
        end
    end

    maxX = max(points(:, 1));
    maxY = max(points(:, 2));
    grid = repmat(' ', maxY + 1, maxX + 1);

    for i = 1:size(points, 1)
        grid(points(i, 2) + 1, points(i, 1) + 1) = '#';
    end

    for r = 1:size(grid, 1)
        fprintf('%s\n', grid(r, :));
    end
end
