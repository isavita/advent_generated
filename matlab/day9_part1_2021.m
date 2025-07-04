
function main()
    fid = fopen('input.txt', 'r');
    heightmap = fscanf(fid, '%1d');
    fclose(fid);

    heightmap = reshape(heightmap, [100, 100])';

    [rows, cols] = size(heightmap);
    totalRiskLevel = 0;

    for y = 1:rows
        for x = 1:cols
            height = heightmap(y, x);
            isLowPoint = true;

            if x > 1 && heightmap(y, x-1) <= height
                isLowPoint = false;
            end
            if x < cols && heightmap(y, x+1) <= height
                isLowPoint = false;
            end
            if y > 1 && heightmap(y-1, x) <= height
                isLowPoint = false;
            end
            if y < rows && heightmap(y+1, x) <= height
                isLowPoint = false;
            end

            if isLowPoint
                totalRiskLevel = totalRiskLevel + 1 + height;
            end
        end
    end

    fprintf('%d\n', totalRiskLevel);
end

main();
