
local file = io.open("input.txt", "r")
local lines = {}
for line in file:lines() do
    table.insert(lines, line)
end
file:close()

local slopes = {
    {1, 1},
    {3, 1},
    {5, 1},
    {7, 1},
    {1, 2}
}

local product = 1
for _, slope in ipairs(slopes) do
    local treeCount = 0
    local pos = 0
    for i = 1, #lines, slope[2] do
        if lines[i]:sub(pos+1, pos+1) == "#" then
            treeCount = treeCount + 1
        end
        pos = (pos + slope[1]) % #lines[i]
    end
    product = product * treeCount
end

print(product)
