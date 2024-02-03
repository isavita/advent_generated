
local file = io.open("input.txt", "r")
local caloriesList = {}
local currentCalories = 0

for line in file:lines() do
    if line == "" then
        table.insert(caloriesList, currentCalories)
        currentCalories = 0
    else
        currentCalories = currentCalories + tonumber(line)
    end
end

table.insert(caloriesList, currentCalories)
table.sort(caloriesList, function(a, b) return a > b end)

local topThreeSum = 0
for i = 1, math.min(3, #caloriesList) do
    topThreeSum = topThreeSum + caloriesList[i]
end

print(topThreeSum)
