
function solve()
    grid_size = 71;
    corrupted = containers.Map('KeyType', 'char', 'ValueType', 'logical');
    
    fid = fopen('input.txt', 'r');
    for i = 1:1024
        line = fgetl(fid);
        if line == -1
            break;
        end
        coords = strsplit(line, ',');
        x = str2double(coords{1});
        y = str2double(coords{2});
        corrupted([num2str(x) ',' num2str(y)]) = true;
    end
    fclose(fid);

    q = [[0, 0, 0]];
    visited = containers.Map('KeyType', 'char', 'ValueType', 'logical');
    visited('0,0') = true;

    while ~isempty(q)
        current = q(1,:);
        q(1,:) = [];
        
        x = current(1);
        y = current(2);
        steps = current(3);

        if x == grid_size - 1 && y == grid_size - 1
            disp(steps);
            return;
        end

        moves = [0, 1; 0, -1; 1, 0; -1, 0];
        for i = 1:size(moves, 1)
            nx = x + moves(i,1);
            ny = y + moves(i,2);
            
            if nx >= 0 && nx < grid_size && ny >= 0 && ny < grid_size && ~isKey(corrupted, [num2str(nx) ',' num2str(ny)]) && ~isKey(visited, [num2str(nx) ',' num2str(ny)])
                q = [q; nx, ny, steps + 1];
                visited([num2str(nx) ',' num2str(ny)]) = true;
            end
        end
    end
end

solve();
