
local file = io.open("input.txt", "r")
local mulCount = 0
local pointer = 1
local registers = {}
local instructions = {}

while true do
    local line = file:read()
    if line == nil then
        break
    end
    table.insert(instructions, line)
end

local function getValue(s)
    if tonumber(s) ~= nil then
        return tonumber(s)
    else
        return registers[s] or 0
    end
end

while pointer >= 1 and pointer <= #instructions do
    local parts = {}
    for part in instructions[pointer]:gmatch("%S+") do
        table.insert(parts, part)
    end
    local cmd, x, y = parts[1], parts[2], parts[3]

    if cmd == "set" then
        registers[x] = getValue(y)
    elseif cmd == "sub" then
        registers[x] = (registers[x] or 0) - getValue(y)
    elseif cmd == "mul" then
        registers[x] = (registers[x] or 0) * getValue(y)
        mulCount = mulCount + 1
    elseif cmd == "jnz" then
        if getValue(x) ~= 0 then
            pointer = pointer + getValue(y) - 1
        end
    end
    pointer = pointer + 1
end

print(mulCount)
