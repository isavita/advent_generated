function abs(x)
    return (x < 0) and -x or x
end

function sign(x)
    if x > 0 then
        return 1
    elseif x < 0 then
        return -1
    else
        return 0
    end
end

local file = io.open("input.txt", "r")
if not file then
    error("Failed to open input file")
end

local lines = {}
for line in file:lines() do
    local parts = {}
    for part in line:gmatch("[^->]+") do
        table.insert(parts, part)
    end

    local start = {}
    local end_ = {}
    for num in parts[1]:gmatch("%d+") do
        table.insert(start, tonumber(num))
    end
    for num in parts[2]:gmatch("%d+") do
        table.insert(end_, tonumber(num))
    end

    table.insert(lines, {start[1], start[2], end_[1], end_[2]})
end
file:close()

local overlaps = {}
for _, line in ipairs(lines) do
    local x1, y1, x2, y2 = line[1], line[2], line[3], line[4]

    local xStep = sign(x2 - x1)
    local yStep = sign(y2 - y1)
    local steps = abs(x2 - x1) + 1
    if abs(y2 - y1) > abs(x2 - x1) then
        steps = abs(y2 - y1) + 1
    end

    for i = 0, steps - 1 do
        local point = {x1 + i * xStep, y1 + i * yStep}
        local key = table.concat(point, ",")
        overlaps[key] = (overlaps[key] or 0) + 1
    end
end

local count = 0
for _, v in pairs(overlaps) do
    if v > 1 then
        count = count + 1
    end
end

print(count)