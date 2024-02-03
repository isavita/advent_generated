
local file = io.open("input.txt", "r")
local numbers = {}

for line in file:lines() do
    table.insert(numbers, tonumber(line))
end

local invalidNumber = 14360655

for i = 1, #numbers do
    local sum = numbers[i]
    local min = numbers[i]
    local max = numbers[i]
    for j = i + 1, #numbers do
        sum = sum + numbers[j]
        if numbers[j] < min then
            min = numbers[j]
        end
        if numbers[j] > max then
            max = numbers[j]
        end
        if sum == invalidNumber then
            print(min + max)
            return
        elseif sum > invalidNumber then
            break
        end
    end
end
