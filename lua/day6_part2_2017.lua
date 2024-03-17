-- Read input from file
local file = io.open("input.txt", "r")
local input = file:read("*a")
file:close()

-- Convert input to table of integers
local banks = {}
for num in string.gmatch(input, "%d+") do
    table.insert(banks, tonumber(num))
end

-- Initialize variables
local seen = {}
local cycles = 0

-- Redistribution loop
while true do
    -- Convert current banks state to string to store in table
    local state = table.concat(banks, ",")

    -- Check for repeats
    if seen[state] then
        print("The size of the loop is", cycles - seen[state])
        return
    end
    seen[state] = cycles

    -- Find the bank with most blocks
    local max_index = 1
    for i = 2, #banks do
        if banks[i] > banks[max_index] then
            max_index = i
        end
    end

    -- Perform redistribution
    local blocks = banks[max_index]
    banks[max_index] = 0
    for i = 1, blocks do
        banks[(max_index + i - 1) % #banks + 1] = banks[(max_index + i - 1) % #banks + 1] + 1
    end

    -- Increment cycle counter
    cycles = cycles + 1
end