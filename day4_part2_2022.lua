
local file = io.open("input.txt", "r")
local count = 0

for line in file:lines() do
    local pair = {}
    for num in line:gmatch("%d+") do
        table.insert(pair, tonumber(num))
    end

    local left = {pair[1], pair[2]}
    local right = {pair[3], pair[4]}

    if left[1] <= right[2] and left[2] >= right[1] then
        count = count + 1
    end
end

print(count)
file:close()
