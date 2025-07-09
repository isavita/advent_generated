
function main()
    fid = fopen('input.txt', 'r');
    lines = textscan(fid, '%s', 'Delimiter', '\n', 'Whitespace', '');
    fclose(fid);
    grid_str = lines{1};

    rows = length(grid_str);
    if rows == 0
        disp(0);
        return;
    end
    cols = length(grid_str{1});

    grid = char(zeros(rows, cols));
    for i = 1:rows
        grid(i, :) = grid_str{i};
    end

    visited = false(rows, cols);
    totalPrice = 0;

    for r = 1:rows
        for c = 1:cols
            if ~visited(r, c)
                [area, perimeter, visited] = calculateRegion(grid, r, c, visited);
                totalPrice = totalPrice + area * perimeter;
            end
        end
    end
    disp(totalPrice);
end

function [area, perimeter, visited] = calculateRegion(grid, startR, startC, visited)
    [rows, cols] = size(grid);
    charVal = grid(startR, startC);
    area = 0;
    perimeter = 0;

    queue = [startR, startC];
    visited(startR, startC) = true;

    head = 1;
    while head <= size(queue, 1)
        currentR = queue(head, 1);
        currentC = queue(head, 2);
        head = head + 1;

        area = area + 1;

        dr = [0, 0, 1, -1];
        dc = [1, -1, 0, 0];

        for i = 1:4
            nR = currentR + dr(i);
            nC = currentC + dc(i);

            if nR >= 1 && nR <= rows && nC >= 1 && nC <= cols
                if grid(nR, nC) ~= charVal
                    perimeter = perimeter + 1;
                elseif ~visited(nR, nC)
                    visited(nR, nC) = true;
                    queue = [queue; nR, nC];
                end
            else
                perimeter = perimeter + 1;
            end
        end
    end
end
