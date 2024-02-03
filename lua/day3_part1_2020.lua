
local file = io.open("input.txt", "r")
local forest = {}
for line in file:lines() do
    table.insert(forest, line)
end
file:close()

function countTrees(forest, right, down)
    local trees = 0
    local x = 0
    local width = #forest[1]

    for y = 1, #forest, down do
        if string.sub(forest[y], x % width + 1, x % width + 1) == "#" then
            trees = trees + 1
        end
        x = x + right
    end

    return trees
end

local trees = countTrees(forest, 3, 1)
print(trees)
