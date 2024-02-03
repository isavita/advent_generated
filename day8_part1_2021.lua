
local file = io.open("input.txt", "r")
local count = 0

for line in file:lines() do
    parts = {}
    for i in string.gmatch(line, "([^|]+)") do
        table.insert(parts, i)
    end
    _, output = parts[1], parts[2]
    for digit in string.gmatch(output, "%S+") do
        if #digit == 2 or #digit == 4 or #digit == 3 or #digit == 7 then
            count = count + 1
        end
    end
end

print(count)
