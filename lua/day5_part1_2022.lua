-- Function to parse initial stacks from input
local function parseStacks(file)
    local stacks = {}
    for line in file:lines() do
        if line:find("%d") then break end  -- Stop parsing at the line with stack numbers
        for i = 2, #line, 4 do
            local crate = line:sub(i, i)
            if crate ~= ' ' then
                local stackNum = (i - 2) / 4 + 1
                stacks[stackNum] = stacks[stackNum] or {}
                table.insert(stacks[stackNum], 1, crate)  -- Insert at the beginning to build the stack
            end
        end
    end
    return stacks
end

-- Function to execute moves
local function executeMoves(file, stacks)
    for line in file:lines() do
        if line:sub(1,4) == "move" then
            local num, from, to = line:match("move (%d+) from (%d+) to (%d+)")
            num, from, to = tonumber(num), tonumber(from), tonumber(to)
            for _ = 1, num do
                local crate = table.remove(stacks[from])
                table.insert(stacks[to], crate)
            end
        end
    end
end

-- Read input file
local file = io.open("input.txt", "r")

-- Parse initial stacks configuration
local stacks = parseStacks(file)

-- Skip the blank line
file:read("*line")

-- Execute the moves
executeMoves(file, stacks)

-- Find the top crate of each stack
local topCrates = {}
for i, stack in pairs(stacks) do
    table.insert(topCrates, stack[#stack])
end

-- Output the result
print("Top crates:", table.concat(topCrates))

-- Close the file
file:close()
