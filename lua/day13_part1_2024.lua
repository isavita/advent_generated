
function readInput(filename)
    local machines = {}
    local lines = {}
    local f = io.open(filename, "r")
    if not f then
        return machines
    end
    for line in f:lines() do
        line = line:gsub("%s+", " ")
        if line == "" then
            if #lines > 0 then
                table.insert(machines, parseMachine(lines))
                lines = {}
            end
        else
            table.insert(lines, line)
        end
    end
    if #lines > 0 then
        table.insert(machines, parseMachine(lines))
    end
    f:close()
    return machines
end

function parseMachine(lines)
    local m = {ax=0, ay=0, bx=0, by=0, px=0, py=0}
    for _, l in ipairs(lines) do
        l = l:gsub("Button A:", "A:")
        l = l:gsub("Button B:", "B:")
        l = l:gsub("Prize:", "P:")
        if l:find("^A:") then
            local x, y = parseLine(l:sub(3))
            m.ax, m.ay = x, y
        elseif l:find("^B:") then
            local x, y = parseLine(l:sub(3))
            m.bx, m.by = x, y
        elseif l:find("^P:") then
            local x, y = parsePrize(l:sub(3))
            m.px, m.py = x, y
        end
    end
    return m
end

function parseLine(s)
    local x, y = 0, 0
    local parts = {}
    for part in s:gmatch("%s*([^,]+)%s*") do
        table.insert(parts, part)
    end
    if #parts >= 2 then
        x = parseVal(parts[1])
        y = parseVal(parts[2])
    end
    return x, y
end

function parsePrize(s)
    local x, y = 0, 0
    local parts = {}
    for part in s:gmatch("%s*([^,]+)%s*") do
        table.insert(parts, part)
    end
    if #parts >= 2 then
        x = parseValPrize(parts[1])
        y = parseValPrize(parts[2])
    end
    return x, y
end

function parseVal(s)
    s = s:gsub("^X%+?", "")
    s = s:gsub("^Y%+?", "")
    s = s:gsub("^X%=?", "")
    s = s:gsub("^Y%=?", "")
    local v = tonumber(s)
    return v or 0
end

function parseValPrize(s)
    s = s:gsub("^X%=?", "")
    s = s:gsub("^Y%=?", "")
    local v = tonumber(s)
    return v or 0
end

function solveMachine(m)
    local minCost = -1
    for aCount = 0, 100 do
        for bCount = 0, 100 do
            local x = m.ax * aCount + m.bx * bCount
            local y = m.ay * aCount + m.by * bCount
            if x == m.px and y == m.py then
                local cost = aCount * 3 + bCount
                if minCost < 0 or cost < minCost then
                    minCost = cost
                end
            end
        end
    end
    return minCost
end

local machines = readInput("input.txt")
local results = {}
for _, m in ipairs(machines) do
    local cost = solveMachine(m)
    if cost >= 0 then
        table.insert(results, cost)
    end
end

if #results == 0 then
    print("0 0")
else
    local sum = 0
    for _, c in ipairs(results) do
        sum = sum + c
    end
    print(string.format("%d %d", #results, sum))
end
