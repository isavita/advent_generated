
function main()
    favoriteNumber = 1362;
    start = [1, 1];
    maxSteps = 50;

    visited = false(100, 100);
    queue = [start];
    visited(start(1)+1, start(2)+1) = true;
    count = 1;

    for step = 1:maxSteps
        newQueue = [];
        for i = 1:size(queue, 1)
            current = queue(i, :);
            deltas = [1, 0; -1, 0; 0, 1; 0, -1];
            for j = 1:4
                next = current + deltas(j, :);
                if all(next >= 0) && next(1) < 100 && next(2) < 100 && ~isWall(next(1), next(2), favoriteNumber) && ~visited(next(1)+1, next(2)+1)
                    visited(next(1)+1, next(2)+1) = true;
                    newQueue = [newQueue; next];
                    count = count + 1;
                end
            end
        end
        queue = newQueue;
        if isempty(queue)
            break;
        end
    end

    fprintf('%d\n', count);
end

function wall = isWall(x, y, favoriteNumber)
    num = x^2 + 3*x + 2*x*y + y + y^2 + favoriteNumber;
    wall = mod(sum(dec2bin(num) - '0'), 2) ~= 0;
end

main();
