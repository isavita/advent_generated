-- Read input from "input.txt"
local file = io.open("input.txt", "r")
local lines = {}
for line in file:lines() do
    table.insert(lines, line)
end
file:close()

-- Parse the input
local initial_state = lines[1]:match("initial state: (.+)")
local rules = {}
for i = 3, #lines do
    local pattern, result = lines[i]:match("(.+) => (.+)")
    rules[pattern] = result
end

-- Initialize the state
local state = {}
for i = 1, #initial_state do
    if initial_state:sub(i, i) == "#" then
        state[i - 1] = "#"
    end
end

-- Simulate 20 generations
for generation = 1, 20 do
    local new_state = {}
    local min_pot, max_pot = 0, 0
    for i, v in pairs(state) do
        min_pot = math.min(min_pot, i)
        max_pot = math.max(max_pot, i)
    end
    for i = min_pot - 2, max_pot + 2 do
        local pattern = ""
        for j = i - 2, i + 2 do
            pattern = pattern .. (state[j] or ".")
        end
        if rules[pattern] == "#" then
            new_state[i] = "#"
        end
    end
    state = new_state
end

-- Calculate the sum of all pot indices with a plant
local sum = 0
for i, v in pairs(state) do
    sum = sum + i
end

-- Print the answer
print(sum)