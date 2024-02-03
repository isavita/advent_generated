
local file = io.open("input.txt", "r")
local input = file:read("*line")
file:close()

local function repeatInput(input, times)
    local digits = {}
    for t = 1, times do
        for i = 1, #input do
            digits[(t - 1) * #input + i] = tonumber(input:sub(i, i))
        end
    end
    return digits
end

local repeatedInput = repeatInput(input, 10000)
local offset = tonumber(input:sub(1, 7))

for phase = 1, 100 do
    local sum = 0
    for i = #repeatedInput, offset, -1 do
        sum = sum + repeatedInput[i]
        repeatedInput[i] = sum % 10
    end
end

for i = offset + 1, offset + 8 do
    io.write(repeatedInput[i])
end
print()
