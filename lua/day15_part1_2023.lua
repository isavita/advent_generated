
local hashTableSize = 256

local function hashString(str)
    local res = 0
    for i = 1, #str do
        local char = string.byte(str, i)
        res = res + char
        res = res * 17
        res = res % hashTableSize
    end
    return res
end

local function parseStep(stepStr)
    local step = {}
    step.Label = stepStr:gsub("=-0123456789", "")
    step.NumBox = hashString(step.Label)
    step.Operation = stepStr:sub(#step.Label + 1, #step.Label + 1)
    if step.Operation == "=" then
        step.Number = tonumber(stepStr:sub(#step.Label + 2))
    end
    return step
end

local function solve(input)
    local line = input[1]
    local steps = string.gmatch(line, "([^,]+)")
    local res = 0
    for step in steps do
        res = res + hashString(step)
    end
    return res
end

local function readFile(fileName)
    local file = io.open(fileName, "r")
    if not file then
        error("Error opening file")
    end
    local content = file:read("*all")
    file:close()
    return content:match("(.-)\n?$")
end

local input = readFile("input.txt")
print(solve({input}))
