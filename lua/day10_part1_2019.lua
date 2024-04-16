function readAsteroids(filename)
    local asteroids = {}
    for line in io.lines(filename) do
        local row = {}
        for i = 1, #line do
            local char = line:sub(i, i)
            table.insert(row, char == '#')
        end
        table.insert(asteroids, row)
    end
    return asteroids
end

function findBestAsteroidLocation(asteroids)
    local maxCount = 0
    for y = 1, #asteroids do
        for x = 1, #asteroids[y] do
            if asteroids[y][x] then
                local count = countVisibleAsteroids(asteroids, x, y)
                if count > maxCount then
                    maxCount = count
                end
            end
        end
    end
    return maxCount
end

function countVisibleAsteroids(asteroids, x, y)
    local angles = {}
    for otherY = 1, #asteroids do
        for otherX = 1, #asteroids[otherY] do
            if asteroids[otherY][otherX] and not (otherX == x and otherY == y) then
                local dx = otherX - x
                local dy = otherY - y
                local angle = math.atan2(dy, dx)
                angles[angle] = true
            end
        end
    end
    local count = 0
    for _ in pairs(angles) do count = count + 1 end
    return count
end

local asteroids = readAsteroids("input.txt")
local maxCount = findBestAsteroidLocation(asteroids)
print(maxCount)