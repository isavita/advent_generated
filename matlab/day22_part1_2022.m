
function main()
    fid = fopen('input.txt', 'r');
    if fid == -1
        error('Error opening file');
    end

    mapLines = {};
    line = fgetl(fid);
    while ~feof(fid) && ~isempty(line) && line(1) ~= char(13) && line(1) ~= char(10)
        mapLines{end+1} = line;
        line = fgetl(fid);
    end

    path = fgetl(fid);
    fclose(fid);

    if isempty(mapLines) || isempty(path)
        error('Invalid input format');
    end

    numRows = length(mapLines);
    maxWidth = 0;
    for i = 1:numRows
        maxWidth = max(maxWidth, length(mapLines{i}));
    end
    numCols = maxWidth;

    grid = repmat(' ', numRows, numCols);
    for i = 1:numRows
        rowStr = mapLines{i};
        grid(i, 1:length(rowStr)) = rowStr;
    end

    rowBoundaries = zeros(numRows, 2);
    colBoundaries = zeros(numCols, 2);

    for y = 1:numRows
        row = grid(y, :);
        nonSpaceCols = find(row ~= ' ');
        if ~isempty(nonSpaceCols)
            rowBoundaries(y, 1) = min(nonSpaceCols);
            rowBoundaries(y, 2) = max(nonSpaceCols);
        end
    end

    for x = 1:numCols
        col = grid(:, x)';
        nonSpaceRows = find(col ~= ' ');
        if ~isempty(nonSpaceRows)
            colBoundaries(x, 1) = min(nonSpaceRows);
            colBoundaries(x, 2) = max(nonSpaceRows);
        end
    end

    startCol = find(grid(1, :) == '.', 1);
    if isempty(startCol)
        error('Starting position not found');
    end
    currentPos = [startCol, 1]; % [x, y]

    facing = 1; % 1: Right, 2: Down, 3: Left, 4: Up

    pathIdx = 1;
    while pathIdx <= length(path)
        if isstrprop(path(pathIdx), 'digit')
            steps = 0;
            while pathIdx <= length(path) && isstrprop(path(pathIdx), 'digit')
                steps = steps * 10 + str2double(path(pathIdx));
                pathIdx = pathIdx + 1;
            end

            for i = 1:steps
                nextPos = currentPos;
                dx = [1, 0, -1, 0]; % R, D, L, U
                dy = [0, 1, 0, -1]; % R, D, L, U

                nextPos(1) = nextPos(1) + dx(facing);
                nextPos(2) = nextPos(2) + dy(facing);

                currentYIdx = currentPos(2);
                currentXIdx = currentPos(1);

                if facing == 1 % Right
                    if nextPos(1) > rowBoundaries(currentYIdx, 2)
                        nextPos(1) = rowBoundaries(currentYIdx, 1);
                    end
                elseif facing == 3 % Left
                    if nextPos(1) < rowBoundaries(currentYIdx, 1)
                        nextPos(1) = rowBoundaries(currentYIdx, 2);
                    end
                elseif facing == 2 % Down
                    if nextPos(2) > colBoundaries(currentXIdx, 2)
                        nextPos(2) = colBoundaries(currentXIdx, 1);
                    end
                elseif facing == 4 % Up
                    if nextPos(2) < colBoundaries(currentXIdx, 1)
                        nextPos(2) = colBoundaries(currentXIdx, 2);
                    end
                end

                nextTile = grid(nextPos(2), nextPos(1));

                if nextTile == '#'
                    break;
                elseif nextTile == '.'
                    currentPos = nextPos;
                end
            end
        elseif path(pathIdx) == 'L' || path(pathIdx) == 'R'
            if path(pathIdx) == 'R'
                facing = mod(facing, 4) + 1;
            else % 'L'
                facing = mod(facing - 2, 4) + 1;
            end
            pathIdx = pathIdx + 1;
        else
            pathIdx = pathIdx + 1;
        end
    end

    password = 1000 * currentPos(2) + 4 * currentPos(1) + (facing - 1);
    fprintf('%d\n', password);
end
