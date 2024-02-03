
-- Step 1: Read Input
local file = io.open("input.txt", "r")
local lines = {}
for line in file:lines() do
    table.insert(lines, tonumber(line))
end
file:close()

-- Step 2: Initialize Variables
local index = 1
local steps = 0

-- Step 3: Navigate Maze
while index >= 1 and index <= #lines do
    -- Fetch the jump offset at the current index
    local jump = lines[index]

    -- Step 4: Update Offset
    lines[index] = lines[index] + 1

    -- Move to the new index
    index = index + jump

    -- Increment steps counter
    steps = steps + 1
end

-- Step 6: Output
print(steps)
