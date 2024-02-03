
local file = io.open("input.txt", "r")
local expenses = {}
for line in file:lines() do
    table.insert(expenses, tonumber(line))
end
file:close()

for i = 1, #expenses do
    for j = i + 1, #expenses do
        for k = j + 1, #expenses do
            if expenses[i] + expenses[j] + expenses[k] == 2020 then
                print(expenses[i] * expenses[j] * expenses[k])
                return
            end
        end
    end
end
