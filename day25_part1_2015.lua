
local file = io.open("input.txt", "r")
local data = file:read("*all")
file:close()

local row, column = string.match(data, "row (%d+), column (%d+)")
row = tonumber(row)
column = tonumber(column)

local function getPosition(row, column)
    return (row + column - 2) * (row + column - 1) / 2 + column
end

local function getCode(position)
    local startCode = 20151125
    local multiplier = 252533
    local modulus = 33554393

    local code = startCode
    for i = 1, position - 1 do
        code = (code * multiplier) % modulus
    end

    return code
end

local pos = getPosition(row, column)
local code = getCode(pos)

print(code)
