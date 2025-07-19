
function main()
    fid = fopen('input.txt', 'r');
    lines = textscan(fid, '%s', 'Delimiter', '\n');
    fclose(fid);
    grid = char(lines{1});

    [gridHeight, gridWidth] = size(grid);
    parts = struct('n', {});
    partsGrid = zeros(gridHeight, gridWidth);
    partCount = 0;

    for y = 1:gridHeight
        [s, e, ~, m] = regexp(grid(y, :), '\d+');
        for i = 1:numel(s)
            partCount = partCount + 1;
            parts(partCount).n = str2double(m{i});
            partsGrid(y, s(i):e(i)) = partCount;
        end
    end

    totalSum = 0;
    [gearRows, gearCols] = find(grid == '*');

    for i = 1:numel(gearRows)
        y = gearRows(i);
        x = gearCols(i);
        
        y_min = max(1, y - 1);
        y_max = min(gridHeight, y + 1);
        x_min = max(1, x - 1);
        x_max = min(gridWidth, x + 1);
        
        window = partsGrid(y_min:y_max, x_min:x_max);
        
        neighbor_part_indices = unique(window(window > 0));
        
        if numel(neighbor_part_indices) == 2
            p1_idx = neighbor_part_indices(1);
            p2_idx = neighbor_part_indices(2);
            totalSum = totalSum + parts(p1_idx).n * parts(p2_idx).n;
        end
    end

    fprintf('%d\n', totalSum);
end
