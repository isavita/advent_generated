
local masses = {}
local total = 0

local file = io.open("input.txt", "r")
for line in file:lines() do
    table.insert(masses, tonumber(line))
end
file:close()

for i = 1, #masses do
    total = total + (math.floor(masses[i] / 3) - 2)
end

print(total)
