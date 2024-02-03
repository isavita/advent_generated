
-- Read Input
local file = io.open("input.txt", "r")
local offsets = {}
for line in file:lines() do
    table.insert(offsets, tonumber(line))
end
file:close()

-- Initialize Variables
local index = 1
local steps = 0

-- Navigate Maze
while index >= 1 and index <= #offsets do
    -- Fetch the jump offset at the current index
    local jump = offsets[index]

    -- Update Offset
    if jump >= 3 then
        offsets[index] = offsets[index] - 1
    else
        offsets[index] = offsets[index] + 1
    end

    -- Move to the new index
    index = index + jump

    -- Increment steps counter
    steps = steps + 1
end

-- Output
print(steps)
