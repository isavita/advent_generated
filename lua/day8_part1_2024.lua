
local file = io.open("input.txt", "r")
local grid = {}
for line in file:lines() do
    table.insert(grid, line)
end
file:close()

local h = #grid
local w = #grid[1]
local antennas = {}
for y = 1, h do
    for x = 1, w do
        local c = string.sub(grid[y], x, x)
        if c ~= "." then
            if not antennas[c] then
                antennas[c] = {}
            end
            table.insert(antennas[c], {y, x})
        end
    end
end

local antinodes = {}
for _, coords in pairs(antennas) do
    local n = #coords
    for i = 1, n do
        for j = i + 1, n do
            local A = coords[i]
            local B = coords[j]
            local P1 = {2*A[1] - B[1], 2*A[2] - B[2]}
            local P2 = {2*B[1] - A[1], 2*B[2] - A[2]}
            if P1[1] >= 1 and P1[1] <= h and P1[2] >= 1 and P1[2] <= w then
                antinodes[P1[1] .. "," .. P1[2]] = true
            end
            if P2[1] >= 1 and P2[1] <= h and P2[2] >= 1 and P2[2] <= w then
                antinodes[P2[1] .. "," .. P2[2]] = true
            end
        end
    end
end

local count = 0
for _ in pairs(antinodes) do
  count = count + 1
end
print(count)
