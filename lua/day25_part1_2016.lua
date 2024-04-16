function getValue(s, registers)
    local num = tonumber(s)
    if num then
        return num
    else
        return registers[s]
    end
end

function producesClockSignal(a, instructions)
    local registers = {a = a, b = 0, c = 0, d = 0}
    local lastOutput = nil
    local outputCount = 0

    local i = 1
    while i <= #instructions do
        local parts = {}
        for part in string.gmatch(instructions[i], "%S+") do
            table.insert(parts, part)
        end

        local cmd = parts[1]
        if cmd == "cpy" then
            local val = getValue(parts[2], registers)
            registers[parts[3]] = val
        elseif cmd == "inc" then
            registers[parts[2]] = registers[parts[2]] + 1
        elseif cmd == "dec" then
            registers[parts[2]] = registers[parts[2]] - 1
        elseif cmd == "jnz" then
            local val = getValue(parts[2], registers)
            if val ~= 0 then
                local jump = tonumber(parts[3])
                i = i + jump
                goto continue
            end
        elseif cmd == "out" then
            local val = getValue(parts[2], registers)
            if val ~= 0 and val ~= 1 then
                return false
            end
            if outputCount > 0 and val == lastOutput then
                return false
            end
            lastOutput = val
            outputCount = outputCount + 1
            if outputCount > 50 then
                return true
            end
        end
        i = i + 1
        ::continue::
    end
    return false
end

local file = io.open("input.txt", "r")
local instructions = {}
if file then
    for line in file:lines() do
        table.insert(instructions, line)
    end
    file:close()
end

local a = 1
while true do
    if producesClockSignal(a, instructions) then
        print(a)
        break
    end
    a = a + 1
end