
local file = io.open("input.txt", "r")
local data = file:read("*all")
file:close()

local frequencyChanges = {}
for change in data:gmatch("[^\n]+") do
    table.insert(frequencyChanges, tonumber(change))
end

local frequencies = {}
local currentFrequency = 0
frequencies[currentFrequency] = true

while true do
    for _, change in ipairs(frequencyChanges) do
        currentFrequency = currentFrequency + change
        if frequencies[currentFrequency] then
            print(currentFrequency)
            return
        end
        frequencies[currentFrequency] = true
    end
end
