
function main
    fid = fopen('input.txt', 'r');
    points = containers.Map('KeyType', 'char', 'ValueType', 'logical');
    folds = {};
    readingPoints = true;

    while ~feof(fid)
        line = strtrim(fgets(fid));
        if isempty(line)
            readingPoints = false;
            continue;
        end

        if readingPoints
            coords = strsplit(line, ',');
            x = str2double(coords{1});
            y = str2double(coords{2});
            points(sprintf('%d,%d', x, y)) = true;
        else
            folds{end+1} = line;
        end
    end
    fclose(fid);

    foldLine = folds{1};
    parts = strsplit(foldLine, ' ');
    foldInstruction = parts{3};
    axisValue = strsplit(foldInstruction, '=');
    axis = axisValue{1};
    value = str2double(axisValue{2});

    newPoints = containers.Map('KeyType', 'char', 'ValueType', 'logical');
    keys = points.keys;

    if strcmp(axis, 'x')
        for i = 1:length(keys)
            key = keys{i};
            coords = str2double(strsplit(key, ','));
            x = coords(1);
            y = coords(2);
            if x > value
                x = 2 * value - x;
            end
            newPoints(sprintf('%d,%d', x, y)) = true;
        end
    else
        for i = 1:length(keys)
            key = keys{i};
            coords = str2double(strsplit(key, ','));
            x = coords(1);
            y = coords(2);
            if y > value
                y = 2 * value - y;
            end
            newPoints(sprintf('%d,%d', x, y)) = true;
        end
    end

    disp(newPoints.Count);
end
