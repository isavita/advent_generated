
function solve()
    SIZE = 71;

    grid = false(SIZE, SIZE);

    fid = fopen('input.txt', 'r');
    if fid == -1
        error('Failed to open input.txt');
    end

    while ~feof(fid)
        line = fgetl(fid);
        if ischar(line)
            coords = sscanf(line, '%d,%d');

            if numel(coords) == 2
                x = coords(1);
                y = coords(2);

                if x >= 0 && x < SIZE && y >= 0 && y < SIZE
                    grid(y + 1, x + 1) = true;
                end

                if ~canReach(grid, SIZE)
                    fprintf('%d,%d\n', x, y);
                    fclose(fid);
                    return;
                end
            end
        end
    end

    fprintf('No cutoff found\n');
    fclose(fid);
end

function reachable = canReach(grid, SIZE)
    if grid(1, 1) || grid(SIZE, SIZE)
        reachable = false;
        return;
    end

    dirs = [1 0; -1 0; 0 1; 0 -1];

    visited = false(SIZE, SIZE);

    queue = zeros(SIZE * SIZE, 2);
    head = 1;
    tail = 1;

    queue(tail, :) = [1, 1];
    visited(1, 1) = true;

    while head <= tail
        current_r = queue(head, 1);
        current_c = queue(head, 2);
        head = head + 1;

        if current_r == SIZE && current_c == SIZE
            reachable = true;
            return;
        end

        for i = 1:4
            nr = current_r + dirs(i, 1);
            nc = current_c + dirs(i, 2);

            if nr >= 1 && nc >= 1 && nr <= SIZE && nc <= SIZE && ...
               ~grid(nr, nc) && ~visited(nr, nc)
                visited(nr, nc) = true;
                tail = tail + 1;
                queue(tail, :) = [nr, nc];
            end
        end
    end

    reachable = false;
end
