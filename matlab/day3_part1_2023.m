
function main()
    matrix = readFileToMatrix('input.txt');
    sum = sumOfPartNumbers(matrix);
    disp(sum);
end

function matrix = readFileToMatrix(filePath)
    fid = fopen(filePath, 'r');
    lines = textscan(fid, '%s', 'Delimiter', '\n');
    fclose(fid);
    
    numRows = length(lines{1});
    numCols = length(lines{1}{1});
    
    matrix = cell(numRows, numCols);
    for i = 1:numRows
        line = lines{1}{i};
        for j = 1:numCols
            matrix{i, j} = line(j);
        end
    end
end

function totalSum = sumOfPartNumbers(matrix)
    [numRows, numCols] = size(matrix);
    visited = false(numRows, numCols);
    totalSum = 0;
    
    for y = 1:numRows
        for x = 1:numCols
            if ~visited(y, x) && ~isnan(str2double(matrix{y, x}))
                [numberStr, numLen] = extractNumber(matrix, x, y);
                if isAdjacentToSymbol(matrix, x, y, numLen)
                    totalSum = totalSum + str2double(numberStr);
                end
                for i = 0:(numLen - 1)
                    visited(y, x + i) = true;
                end
            end
        end
    end
end

function [numberStr, numLen] = extractNumber(matrix, x, y)
    [~, numCols] = size(matrix);
    numberStr = '';
    currentX = x;
    while currentX <= numCols && ~isnan(str2double(matrix{y, currentX}))
        numberStr = [numberStr, matrix{y, currentX}];
        currentX = currentX + 1;
    end
    numLen = length(numberStr);
end

function adjacent = isAdjacentToSymbol(matrix, x, y, len)
    adjacent = false;
    for i = 0:(len - 1)
        if checkAdjacent(matrix, x + i, y)
            adjacent = true;
            return;
        end
    end
end

function adjacent = checkAdjacent(matrix, x, y)
    [numRows, numCols] = size(matrix);
    adjacent = false;
    
    for dy = -1:1
        for dx = -1:1
            adjX = x + dx;
            adjY = y + dy;
            
            if adjY >= 1 && adjY <= numRows && adjX >= 1 && adjX <= numCols
                char = matrix{adjY, adjX};
                if isnan(str2double(char)) && ~strcmp(char, '.')
                    adjacent = true;
                    return;
                end
            end
        end
    end
end

main();
