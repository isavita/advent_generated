
function main()
    rules = readRules();
    grid = {'.#.', '..#', '###'};
    gridSize = 3;

    for i = 1:5
        [grid, gridSize] = applyRules(grid, gridSize, rules);
    end

    count = 0;
    for r = 1:gridSize
        for c = 1:gridSize
            if grid{r}(c) == '#'
                count = count + 1;
            end
        end
    end
    disp(count);
end

function rules = readRules()
    fid = fopen('input.txt', 'r');
    rules = struct('input', {}, 'output', {});
    ruleIndex = 1;
    while ~feof(fid)
        line = strtrim(fgetl(fid));
        parts = strsplit(line, ' => ');
        if numel(parts) == 2
            rules(ruleIndex).input = parts{1};
            rules(ruleIndex).output = parts{2};
            ruleIndex = ruleIndex + 1;
        end
    end
    fclose(fid);
end

function [newGrid, newSize] = applyRules(grid, gridSize, rules)
    if mod(gridSize, 2) == 0
        subSize = 2;
        newSize = (gridSize / subSize) * 3;
    else
        subSize = 3;
        newSize = (gridSize / subSize) * 4;
    end

    newGrid = cell(1, newSize);
    newGridIndex = 1;

    for y = 0:subSize:(gridSize - 1)
        for x = 0:subSize:(gridSize - 1)
            square = '';
            for dy = 0:(subSize - 1)
                square = [square, grid{y + dy + 1}(x + 1:(x + subSize))];
                if dy < subSize - 1
                    square = [square, '/'];
                end
            end

            enhancedSquare = enhance(square, subSize, rules);

            outputSubSize = subSize + 1;
            outputRowStart = floor(y / subSize) * outputSubSize;
            outputColStart = floor(x / subSize) * outputSubSize;

            outputSquareParts = strsplit(enhancedSquare, '/');
            for r = 1:outputSubSize
                if isempty(newGrid{outputRowStart + r})
                    newGrid{outputRowStart + r} = repmat(' ', 1, newSize);
                end
                newGrid{outputRowStart + r}(outputColStart + 1 : outputColStart + outputSubSize) = outputSquareParts{r};
            end
        end
    end
    
    for i = 1:newSize
        newGrid{i} = strtrim(newGrid{i});
    end
end

function enhancedSquare = enhance(square, size, rules)
    currentSquare = square;
    
    for i = 1:4
        if any(strcmp({rules.input}, currentSquare))
            ruleIndex = find(strcmp({rules.input}, currentSquare), 1);
            enhancedSquare = rules(ruleIndex).output;
            return;
        end
        currentSquare = rotate(currentSquare, size);
    end

    flippedSquare = flip(currentSquare, size);
    currentSquare = flippedSquare;

    for i = 1:4
        if any(strcmp({rules.input}, currentSquare))
            ruleIndex = find(strcmp({rules.input}, currentSquare), 1);
            enhancedSquare = rules(ruleIndex).output;
            return;
        end
        currentSquare = rotate(currentSquare, size);
    end
    enhancedSquare = ''; 
end

function output = rotate(input, size)
    parts = cell(1, size);
    k = 1;
    for i = 1:size
        parts{i} = input(k : k + size - 1);
        k = k + size + 1;
    end

    output = '';
    for x = 1:size
        for y = size:-1:1
            output = [output, parts{y}(x)];
        end
        if x < size
            output = [output, '/'];
        end
    end
end

function output = flip(input, size)
    parts = cell(1, size);
    k = 1;
    for i = 1:size
        parts{i} = input(k : k + size - 1);
        k = k + size + 1;
    end

    output = '';
    for i = 1:size
        output = [output, fliplr(parts{i})];
        if i < size
            output = [output, '/'];
        end
    end
end

main();
