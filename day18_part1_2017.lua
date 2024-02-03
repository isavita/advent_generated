
local file = io.open("input.txt", "r")
local instructions = {}
for line in file:lines() do
    table.insert(instructions, {})
    for word in line:gmatch("%S+") do
        table.insert(instructions[#instructions], word)
    end
end
file:close()

local registers = {}
local lastSound = 0

local function getValue(arg)
    if tonumber(arg) then
        return tonumber(arg)
    else
        return registers[arg] or 0
    end
end

local i = 1
while i <= #instructions do
    local instruction = instructions[i]
    local cmd = instruction[1]
    local arg1 = instruction[2]

    if cmd == "snd" then
        lastSound = getValue(arg1)
    elseif cmd == "set" then
        registers[arg1] = getValue(instruction[3])
    elseif cmd == "add" then
        registers[arg1] = (registers[arg1] or 0) + getValue(instruction[3])
    elseif cmd == "mul" then
        registers[arg1] = (registers[arg1] or 0) * getValue(instruction[3])
    elseif cmd == "mod" then
        registers[arg1] = (registers[arg1] or 0) % getValue(instruction[3])
    elseif cmd == "rcv" then
        if getValue(arg1) ~= 0 then
            print(lastSound)
            return
        end
    elseif cmd == "jgz" then
        if getValue(arg1) > 0 then
            i = i + getValue(instruction[3])
            goto continue
        end
    end
    i = i + 1
    ::continue::
end
