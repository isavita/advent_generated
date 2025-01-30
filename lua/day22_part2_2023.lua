
local lines = {}
for line in io.lines("input.txt") do
    if line ~= "" then
        table.insert(lines, line)
    end
end

local function parseLine(line)
    local miniStr, maxiStr = line:match("(%d+,%d+,%d+)~(%d+,%d+,%d+)")
    local mini = {}
    local miniParts = {}
    for part in miniStr:gmatch("%d+") do
        table.insert(miniParts, tonumber(part))
    end
    mini.x, mini.y, mini.z = miniParts[1], miniParts[2], miniParts[3]
    local maxi = {}
    local maxiParts = {}
    for part in maxiStr:gmatch("%d+") do
        table.insert(maxiParts, tonumber(part))
    end
    maxi.x, maxi.y, maxi.z = maxiParts[1], maxiParts[2], maxiParts[3]
    local brick = {
        mini = mini,
        maxi = maxi,
        basedOn = {},
        support = {},
        id = nil,
    }
    return brick
end

local bricks = {}
for _, line in ipairs(lines) do
    local brick = parseLine(line)
    table.insert(bricks, brick)
end

for i, brick in ipairs(bricks) do
    brick.id = i
end

table.sort(bricks, function(a, b)
    return a.maxi.z < b.maxi.z
end)

for i, currentBrick in ipairs(bricks) do
    local supportZ = 0
    local basedOn = {}

    for j = i - 1, 1, -1 do
        local otherBrick = bricks[j]

        local xOverlap = currentBrick.mini.x <= otherBrick.maxi.x and otherBrick.mini.x <= currentBrick.maxi.x
        local yOverlap = currentBrick.mini.y <= otherBrick.maxi.y and otherBrick.mini.y <= currentBrick.maxi.y
        if xOverlap and yOverlap then
            if otherBrick.maxi.z > supportZ then
                supportZ = otherBrick.maxi.z
                basedOn = {otherBrick}
            elseif otherBrick.maxi.z == supportZ then
                table.insert(basedOn, otherBrick)
            end
        end
    end

    currentBrick.basedOn = basedOn
    for _, brick in ipairs(basedOn) do
        table.insert(brick.support, currentBrick)
    end

    local deltaZ = currentBrick.maxi.z - currentBrick.mini.z
    currentBrick.mini.z = supportZ + 1
    currentBrick.maxi.z = currentBrick.mini.z + deltaZ
end

local function countTable(t)
    local count = 0
    for _ in pairs(t) do
        count = count + 1
    end
    return count
end

local function solve(bricks)
    local cnt = 0
    for _, currentBrick in ipairs(bricks) do
        local falling = {}
        for _, supportedBrick in ipairs(currentBrick.support) do
            if #supportedBrick.basedOn == 1 then
                local toProcess = {supportedBrick}
                while #toProcess > 0 do
                    local brick0 = table.remove(toProcess, 1)
                    local allBasedOnFalling = true
                    for _, basedBrick in ipairs(brick0.basedOn) do
                        if basedBrick ~= currentBrick and not falling[basedBrick.id] then
                            allBasedOnFalling = false
                            break
                        end
                    end
                    if allBasedOnFalling then
                        if not falling[brick0.id] then
                            falling[brick0.id] = true
                            for _, s in ipairs(brick0.support) do
                                table.insert(toProcess, s)
                            end
                        end
                    end
                end
            end
        end
        cnt = cnt + countTable(falling)
    end
    return cnt
end

print(solve(bricks))
