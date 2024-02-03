
function parseInput(input)
    local bricks = {}
    for i, line in ipairs(input) do
        local brick = {
            mini = {},
            maxi = {},
            basedOn = {},
            support = {}
        }
        local values = {}
        for value in string.gmatch(line, "([^~]+)") do
            table.insert(values, value)
        end
        local miniValues = {}
        local maxiValues = {}
        for value in string.gmatch(values[1], "(%d+)") do
            table.insert(miniValues, tonumber(value))
        end
        for value in string.gmatch(values[2], "(%d+)") do
            table.insert(maxiValues, tonumber(value))
        end
        brick.mini.x = miniValues[1]
        brick.mini.y = miniValues[2]
        brick.mini.z = miniValues[3]
        brick.maxi.x = maxiValues[1]
        brick.maxi.y = maxiValues[2]
        brick.maxi.z = maxiValues[3]
        table.insert(bricks, brick)
    end
    return bricks
end

function settle(bricks)
    table.sort(bricks, function(a, b)
        return a.maxi.z < b.maxi.z
    end)

    for i, brick in ipairs(bricks) do
        local supportZ = 0
        local basedBricks = {}

        for j = i - 1, 1, -1 do
            local isIntersectingX = math.max(brick.mini.x, bricks[j].mini.x) <= math.min(brick.maxi.x, bricks[j].maxi.x)
            local isIntersectingY = math.max(brick.mini.y, bricks[j].mini.y) <= math.min(brick.maxi.y, bricks[j].maxi.y)
            local isIntersecting = isIntersectingX and isIntersectingY
            if isIntersecting then
                if bricks[j].maxi.z == supportZ then
                    table.insert(basedBricks, bricks[j])
                elseif bricks[j].maxi.z > supportZ then
                    supportZ = bricks[j].maxi.z
                    basedBricks = {bricks[j]}
                end
            end
        end

        brick.basedOn = basedBricks
        for _, basedBrick in ipairs(basedBricks) do
            table.insert(basedBrick.support, brick)
        end

        local deltaZ = brick.maxi.z - brick.mini.z
        brick.mini.z = supportZ + 1
        brick.maxi.z = brick.mini.z + deltaZ
    end
end

function solve(input)
    local bricks = parseInput(input)
    settle(bricks)

    local cnt = 0
    for _, brick in ipairs(bricks) do
        local isDisintegratable = true
        for _, supportedBrick in ipairs(brick.support) do
            if #supportedBrick.basedOn < 2 then
                isDisintegratable = false
                break
            end
        end
        if isDisintegratable then
            cnt = cnt + 1
        end
    end
    return cnt
end

function readFile(fileName)
    local file = io.open(fileName, "r")
    local content = file:read("*all")
    file:close()
    local lines = {}
    for line in string.gmatch(content, "[^\n]+") do
        table.insert(lines, line)
    end
    return lines
end

local input = readFile("input.txt")
print(solve(input))
