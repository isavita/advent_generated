local function sum_numbers(str)
    local total = 0
    for number in str:gmatch("-?%d+") do
        total = total + tonumber(number)
    end
    return total
end

-- Read input from file
local file = io.open("input.txt", "r")
if not file then
    print("Error: Unable to open input.txt")
    os.exit(1)
end

local content = file:read("*all")
file:close()

-- Calculate sum
local result = sum_numbers(content)

-- Print result
print(result)
