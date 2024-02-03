
function readInput()
    local lines = {}
    local file = io.open("input.txt", "r")
    if not file then
        error("Error opening file")
    end
    for line in file:lines() do
        table.insert(lines, line)
    end
    file:close()
    return lines
end

function parseChange(change)
    local sign, num = parseSignNum(change)
    return sign * num
end

function parseSignNum(change)
    local sign = 1
    if string.sub(change, 1, 1) == "-" then
        sign = -1
        change = string.sub(change, 2)
    end
    local num = tonumber(change)
    if not num then
        error("invalid frequency change: " .. change)
    end
    return sign, num
end

local freqChanges = readInput()
local freq = 0
for i, change in ipairs(freqChanges) do
    freq = freq + parseChange(change)
end
print(freq)
