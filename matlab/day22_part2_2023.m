
function main()
    inputStr = fileread('input.txt');
    
    [bricks, brickCount] = parseInput(inputStr);
    
    result = solve(bricks, brickCount);
    
    fprintf('%d\n', result);
end

function [bricks, brickCount] = parseInput(inputStr)
    lines = strsplit(inputStr, '\n');
    lines = lines(~cellfun('isempty', lines));

    brickCount = numel(lines);
    
    bricks(brickCount).mini.x = 0;
    bricks(brickCount).mini.y = 0;
    bricks(brickCount).mini.z = 0;
    bricks(brickCount).maxi.x = 0;
    bricks(brickCount).maxi.y = 0;
    bricks(brickCount).maxi.z = 0;
    bricks(brickCount).basedOn = [];
    bricks(brickCount).support = [];

    for i = 1:brickCount
        line = lines{i};
        data = textscan(line, '%d,%d,%d~%d,%d,%d');
        
        bricks(i).mini.x = data{1};
        bricks(i).mini.y = data{2};
        bricks(i).mini.z = data{3};
        bricks(i).maxi.x = data{4};
        bricks(i).maxi.y = data{5};
        bricks(i).maxi.z = data{6};
        bricks(i).basedOn = [];
        bricks(i).support = [];
    end
end

function bricks = settle(bricks, brickCount)
    z_max_values = [bricks.maxi];
    z_max_values = [z_max_values.z];
    [~, sortedIndices] = sort(z_max_values);

    for i = 1:brickCount
        currentBrickIdx = sortedIndices(i);
        currentBrick = bricks(currentBrickIdx);

        supportZ = 0;
        basedOnIndices = [];

        for j = 1:(i-1)
            prevBrickIdx = sortedIndices(j);
            prevBrick = bricks(prevBrickIdx);

            isIntersectingX = max(currentBrick.mini.x, prevBrick.mini.x) <= min(currentBrick.maxi.x, prevBrick.maxi.x);
            isIntersectingY = max(currentBrick.mini.y, prevBrick.mini.y) <= min(currentBrick.maxi.y, prevBrick.maxi.y);

            if isIntersectingX && isIntersectingY
                if prevBrick.maxi.z == supportZ
                    basedOnIndices = [basedOnIndices, prevBrickIdx];
                elseif prevBrick.maxi.z > supportZ
                    supportZ = prevBrick.maxi.z;
                    basedOnIndices = prevBrickIdx;
                end
            end
        end
        
        currentBrick.basedOn = basedOnIndices;
        
        for j = 1:numel(basedOnIndices)
            supportedByBrickIdx = basedOnIndices(j);
            bricks(supportedByBrickIdx).support = [bricks(supportedByBrickIdx).support, currentBrickIdx];
        end

        deltaZ = currentBrick.maxi.z - currentBrick.mini.z;
        currentBrick.mini.z = supportZ + 1;
        currentBrick.maxi.z = currentBrick.mini.z + deltaZ;
        
        bricks(currentBrickIdx) = currentBrick;
    end
end

function totalFallingCount = solve(bricks, brickCount)
    bricks = settle(bricks, brickCount);

    totalFallingCount = 0;

    for i = 1:brickCount
        disintegratedBrickIdx = i;

        fallingSet = false(1, brickCount); 
        
        queue = zeros(1, brickCount); 
        head = 1;
        tail = 0;
        
        for j = 1:numel(bricks(disintegratedBrickIdx).support)
            supportedBrickIdx = bricks(disintegratedBrickIdx).support(j);
            if numel(bricks(supportedBrickIdx).basedOn) == 1
                tail = tail + 1;
                queue(tail) = supportedBrickIdx;
            end
        end

        while head <= tail
            currentBrickIdx = queue(head);
            head = head + 1;

            if fallingSet(currentBrickIdx)
                continue;
            end

            wouldFall = true;
            for k = 1:numel(bricks(currentBrickIdx).basedOn)
                basedBrickIdx = bricks(currentBrickIdx).basedOn(k);
                if basedBrickIdx ~= disintegratedBrickIdx && ~fallingSet(basedBrickIdx)
                    wouldFall = false;
                    break;
                end
            end

            if wouldFall
                fallingSet(currentBrickIdx) = true;
                for k = 1:numel(bricks(currentBrickIdx).support)
                    nextSupportedBrickIdx = bricks(currentBrickIdx).support(k);
                    if ~fallingSet(nextSupportedBrickIdx)
                        tail = tail + 1;
                        queue(tail) = nextSupportedBrickIdx;
                    end
                end
            end
        end
        
        totalFallingCount = totalFallingCount + sum(fallingSet);
    end
end

% Call the main function
main();
