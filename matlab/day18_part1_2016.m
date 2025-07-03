
function main
    fid = fopen('input.txt', 'r');
    firstRow = strtrim(fgets(fid));
    fclose(fid);

    rows = {firstRow};
    safeTiles = countSafeTiles(firstRow);

    for i = 1:39
        currentRow = rows{end};
        nextRow = '';
        for j = 1:length(currentRow)
            left = '.';
            if j > 1
                left = currentRow(j-1);
            end
            center = currentRow(j);
            right = '.';
            if j < length(currentRow)
                right = currentRow(j+1);
            end
            nextRow = [nextRow, isTrap(left, center, right)];
        end
        rows{end+1} = nextRow;
        safeTiles = safeTiles + countSafeTiles(nextRow);
    end

    fprintf('%d\n', safeTiles);
end

function result = isTrap(left, center, right)
    if strcmp(left, '^') && strcmp(center, '^') && strcmp(right, '.')
        result = '^';
    elseif strcmp(center, '^') && strcmp(right, '^') && strcmp(left, '.')
        result = '^';
    elseif strcmp(left, '^') && strcmp(center, '.') && strcmp(right, '.')
        result = '^';
    elseif strcmp(right, '^') && strcmp(center, '.') && strcmp(left, '.')
        result = '^';
    else
        result = '.';
    end
end

function count = countSafeTiles(row)
    count = sum(row == '.');
end

main;
