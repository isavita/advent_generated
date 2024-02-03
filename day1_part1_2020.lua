
local file = io.open("input.txt", "r")
local numbers = {}
for line in file:lines() do
    if line ~= "" then
        table.insert(numbers, tonumber(line))
    end
end
file:close()

for i = 1, #numbers-1 do
    for j = i+1, #numbers do
        if numbers[i] + numbers[j] == 2020 then
            print(numbers[i] * numbers[j])
            return
        end
    end
end
