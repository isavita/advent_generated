-- Read input from file
local file = io.open("input.txt", "r")
local input = file:read("*a")
file:close()

-- Parse input
local banks = {}
for num in string.gmatch(input, "%d+") do
    table.insert(banks, tonumber(num))
end

-- Initialize variables
local seen = {}
local cycles = 0

-- Redistribution loop
while true do
    -- Convert current banks state to string to store in set
    local state = table.concat(banks, ",")
    if seen[state] then
        break
    end
    seen[state] = true

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

-- Output
print("It takes", cycles, "redistribution cycles to reach a repeated configuration.")