
local file = io.open("input.txt", "r")
if not file then
    error("Error opening file")
end

local maxCalories = 0
local currentCalories = 0

for line in file:lines() do
    if line == "" then
        if currentCalories > maxCalories then
            maxCalories = currentCalories
        end
        currentCalories = 0
    else
        local calories = tonumber(line)
        if not calories then
            error("Error converting line to int")
        end
        currentCalories = currentCalories + calories
    end
end

if currentCalories > maxCalories then
    maxCalories = currentCalories
end

file:close()

print(maxCalories)
