local function toInt(s)
    return tonumber(s)
end

local file = io.open("input.txt", "r")
local input = file:read("*all")
file:close()

local lines = {}
for line in input:gmatch("[^\r\n]+") do
    table.insert(lines, line)
end

local head = {next = nil}
local tail = head

local pattern = "position=<%s*(-?%d+),%s*(-?%d+)> velocity=<%s*(-?%d+),%s*(-?%d+)>"
for _, line in ipairs(lines) do
    local x, y, vX, vY = line:match(pattern)
    if x and y and vX and vY then
        local star = {x = toInt(x), y = toInt(y), vX = toInt(vX), vY = toInt(vY), next = nil}
        tail.next = star
        tail = star
    end
end

local smallestT = 0
local smallestArea = math.huge
for t = 1, 100000 do
    local maxX, maxY = -math.huge, -math.huge
    local minX, minY = math.huge, math.huge

    local temp = head.next
    while temp do
        local x = temp.x + temp.vX * t
        local y = temp.y + temp.vY * t
        maxX = math.max(maxX, x)
        minX = math.min(minX, x)
        maxY = math.max(maxY, y)
        minY = math.min(minY, y)
        temp = temp.next
    end

    local lenX = maxX - minX + 1
    local lenY = maxY - minY + 1
    local area = lenX * lenY

    if smallestArea > area then
        smallestArea = area
        smallestT = t
    end
end

local t = smallestT
local maxX, maxY = -math.huge, -math.huge
local minX, minY = math.huge, math.huge

local temp = head.next
while temp do
    temp.x = temp.x + temp.vX * t
    temp.y = temp.y + temp.vY * t
    maxX = math.max(maxX, temp.x)
    minX = math.min(minX, temp.x)
    maxY = math.max(maxY, temp.y)
    minY = math.min(minY, temp.y)
    temp = temp.next
end

local mapper = {}
for i = 1, maxY - minY + 1 do
    mapper[i] = {}
    for j = 1, maxX - minX + 1 do
        mapper[i][j] = false
    end
end

local temp = head.next
while temp do
    local x = temp.x - minX + 1
    local y = temp.y - minY + 1
    mapper[y][x] = true
    temp = temp.next
end

for i = 1, #mapper do
    local line = ""
    for j = 1, #mapper[i] do
        if mapper[i][j] then
            line = line .. "#"
        else
            line = line .. " "
        end
    end
    print(line)
end