-- Function to calculate the length of a table
local function tablelength(tbl)
    local count = 0
    for _ in pairs(tbl) do count = count + 1 end
    return count
end

local file = io.open("input.txt", "r")
local input = file:read("*all")
file:close()

local grid = {}
local parts = {}
local curr = nil
local y = 0

-- Process each line from input
for line in input:gmatch("[^\r\n]+") do
    if curr then
        table.insert(parts, curr)
        curr = nil
    end
    for x = 1, #line do
        local c = line:sub(x, x)
        grid[(x-1) .. "," .. y] = c
        if c:match("%d") then
            if not curr then
                curr = {xmin = x-1, xmax = x-1, y = y, n = tonumber(c)}
            else
                curr.n = curr.n * 10 + tonumber(c)
                curr.xmax = x-1
            end
        elseif curr then
            table.insert(parts, curr)
            curr = nil
        end
    end
    y = y + 1
end

if curr then
    table.insert(parts, curr)
end

local partsGrid = {}
for i, p in ipairs(parts) do
    for x = p.xmin, p.xmax do
        partsGrid[x .. "," .. p.y] = i
    end
end

local sum = 0
local Neighbors8 = {
    {0, 1}, {0, -1}, {1, 0}, {-1, 0},
    {-1, -1}, {-1, 1}, {1, -1}, {1, 1}
}

-- Calculate sum based on '*' neighbors
for p, c in pairs(grid) do
    if c == '*' then
        local neighborParts = {}
        local px, py = p:match("^(%-?%d+),(%-?%d+)$")
        px, py = tonumber(px), tonumber(py)
        for _, n in ipairs(Neighbors8) do
            local neighbor = (px + n[1]) .. "," .. (py + n[2])
            local partIndex = partsGrid[neighbor]
            if partIndex then
                neighborParts[partIndex] = true
            end
        end
        if tablelength(neighborParts) == 2 then
            local prod = 1
            for i in pairs(neighborParts) do
                prod = prod * parts[i].n
            end
            sum = sum + prod
        end
    end
end

print(sum)