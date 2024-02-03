
-- Step 1: Read Input
local file = io.open("input.txt", "r")
local lines = {}
for line in file:lines() do
    table.insert(lines, line)
end
file:close()

-- Step 2: Create Data Structure
local holderMap = {}
local heldMap = {}

-- Regular expression to match program names
local re = "[a-z]+"

for _, line in ipairs(lines) do
    local names = {}
    for name in line:gmatch(re) do
        table.insert(names, name)
    end
    local holder = names[1]
    holderMap[holder] = true

    -- If the program holds others, update the held map
    if #names > 1 then
        for i = 2, #names do
            heldMap[names[i]] = true
        end
    end
end

-- Step 3: Find Root
for holder, _ in pairs(holderMap) do
    if not heldMap[holder] then
        print(holder)
        break
    end
end
