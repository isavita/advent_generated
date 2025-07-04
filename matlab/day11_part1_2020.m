
function main()
    fid = fopen('input.txt', 'r');
    seatingArea = textscan(fid, '%s', 'Delimiter', '\n');
    fclose(fid);
    seatingArea = cellfun(@(x) x', seatingArea{1}, 'UniformOutput', false);
    seatingArea = cellfun(@(x) x(:)', seatingArea, 'UniformOutput', false);
    seatingArea = vertcat(seatingArea{:});

    rows = size(seatingArea, 1);
    cols = size(seatingArea, 2);
    directions = [-1 -1; -1 0; -1 1; 0 -1; 0 1; 1 -1; 1 0; 1 1];

    while true
        changes = 0;
        newSeatingArea = seatingArea;
        for i = 1:rows
            for j = 1:cols
                if seatingArea(i, j) == '.'
                    continue
                end

                occupiedNeighbors = 0;
                for k = 1:size(directions, 1)
                    x = i + directions(k, 1);
                    y = j + directions(k, 2);
                    if x >= 1 && x <= rows && y >= 1 && y <= cols && seatingArea(x, y) == '#'
                        occupiedNeighbors = occupiedNeighbors + 1;
                    end
                end

                if seatingArea(i, j) == 'L' && occupiedNeighbors == 0
                    newSeatingArea(i, j) = '#';
                    changes = changes + 1;
                elseif seatingArea(i, j) == '#' && occupiedNeighbors >= 4
                    newSeatingArea(i, j) = 'L';
                    changes = changes + 1;
                end
            end
        end

        if changes == 0
            break
        end
        seatingArea = newSeatingArea;
    end

    occupiedCount = sum(seatingArea(:) == '#');
    fprintf('%d\n', occupiedCount);
end

main();
