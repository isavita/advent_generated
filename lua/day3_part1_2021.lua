-- Read input from file
local file = io.open("input.txt", "r")
local lines = {}
for line in file:lines() do
    table.insert(lines, line)
end
file:close()

-- Count bits
local counts = {}
for i = 1, 12 do
    counts[i] = {0, 0}
end
for _, num in ipairs(lines) do
    for i = 1, #num do
        counts[i][tonumber(num:sub(i, i)) + 1] = counts[i][tonumber(num:sub(i, i)) + 1] + 1
    end
end

-- Calculate gamma and epsilon rates
local gamma_rate = 0
local epsilon_rate = 0
for i = 1, 12 do
    if counts[i][1] > counts[i][2] then
        gamma_rate = gamma_rate + 2 ^ (12 - i)
    else
        epsilon_rate = epsilon_rate + 2 ^ (12 - i)
    end
end

-- Print the answer
print(gamma_rate * epsilon_rate)