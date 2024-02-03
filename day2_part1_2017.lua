
local file = io.open("input.txt", "r")
local data = file:read("*all")
file:close()

local lines = {}
for line in data:gmatch("[^\r\n]+") do
    table.insert(lines, line)
end

local checksum = 0

for _, line in ipairs(lines) do
    local nums = {}
    for num in line:gmatch("%S+") do
        table.insert(nums, tonumber(num))
    end

    local minVal = nums[1]
    local maxVal = nums[1]

    for _, num in ipairs(nums) do
        if num < minVal then
            minVal = num
        end
        if num > maxVal then
            maxVal = num
        end
    end

    checksum = checksum + (maxVal - minVal)
end

print(checksum)
