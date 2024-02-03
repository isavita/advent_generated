
function parseInput(data)
    lines = {}
    for line in io.lines("input.txt") do
        table.insert(lines, line)
    end
    depth = tonumber(string.match(lines[1], "%d+"))
    coords = string.match(lines[2], "%d+,%d+")
    x, y = string.match(coords, "(%d+),(%d+)")
    return depth, {tonumber(x), tonumber(y)}
end

function makeCaveSystem(depth, target)
    cave = {}
    for y = 0, target[2] do
        cave[y] = {}
        for x = 0, target[1] do
            if x == 0 and y == 0 or x == target[1] and y == target[2] then
                geologicIndex = 0
            elseif y == 0 then
                geologicIndex = x * 16807
            elseif x == 0 then
                geologicIndex = y * 48271
            else
                geologicIndex = cave[y][x-1] * cave[y-1][x]
            end
            cave[y][x] = (geologicIndex + depth) % 20183
        end
    end
    return cave
end

function calculateRiskLevel(cave, target)
    riskLevel = 0
    for y = 0, target[2] do
        for x = 0, target[1] do
            riskLevel = riskLevel + cave[y][x] % 3
        end
    end
    return riskLevel
end

depth, target = parseInput()
cave = makeCaveSystem(depth, target)
riskLevel = calculateRiskLevel(cave, target)
print("Total Risk Level:", riskLevel)
