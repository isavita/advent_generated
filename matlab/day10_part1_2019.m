
function main()
    asteroids = readAsteroids("input.txt");
    maxCount = findBestAsteroidLocation(asteroids);
    disp(maxCount);
end

function asteroids = readAsteroids(filename)
    file = fopen(filename, 'r');
    asteroids = [];
    while ~feof(file)
        line = strtrim(fgetl(file));
        asteroidRow = line == '#';
        asteroids = [asteroids; asteroidRow];
    end
    fclose(file);
end

function maxCount = findBestAsteroidLocation(asteroids)
    [rows, cols] = size(asteroids);
    maxCount = 0;
    for y = 1:rows
        for x = 1:cols
            if asteroids(y, x)
                count = countVisibleAsteroids(asteroids, x, y);
                if count > maxCount
                    maxCount = count;
                end
            end
        end
    end
end

function count = countVisibleAsteroids(asteroids, x, y)
    [rows, cols] = size(asteroids);
    angles = [];
    for otherY = 1:rows
        for otherX = 1:cols
            if asteroids(otherY, otherX) && ~(otherX == x && otherY == y)
                angle = atan2(otherY - y, otherX - x);
                angles = [angles, angle];
            end
        end
    end
    count = numel(unique(angles));
end

main();
