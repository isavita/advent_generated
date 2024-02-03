
local file = io.open("input.txt", "r")
local sum = 0

for line in file:lines() do
    local nums = {}
    for numStr in line:gmatch("%S+") do
        table.insert(nums, tonumber(numStr))
    end

    for i, num1 in ipairs(nums) do
        for j, num2 in ipairs(nums) do
            if i ~= j and num1 % num2 == 0 then
                sum = sum + num1 / num2
            end
        end
    end
end

print(sum)
file:close()
