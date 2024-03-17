-- Read input from file
local file = io.open("input.txt", "r")
local instructions = {}
for line in file:lines() do
    table.insert(instructions, line)
end
file:close()

-- Initialize register c to 1
local registers = {a = 0, b = 0, c = 1, d = 0}

local function get_value(s)
    local num = tonumber(s)
    if num then
        return num
    else
        return registers[s]
    end
end

local function execute_instructions(instructions)
    local i = 1
    while i <= #instructions do
        local parts = {}
        for part in string.gmatch(instructions[i], "%S+") do
            table.insert(parts, part)
        end
        if parts[1] == "cpy" then
            registers[parts[3]] = get_value(parts[2])
            i = i + 1
        elseif parts[1] == "inc" then
            registers[parts[2]] = registers[parts[2]] + 1
            i = i + 1
        elseif parts[1] == "dec" then
            registers[parts[2]] = registers[parts[2]] - 1
            i = i + 1
        elseif parts[1] == "jnz" then
            local val = get_value(parts[2])
            if val ~= 0 then
                i = i + tonumber(parts[3])
            else
                i = i + 1
            end
        end
    end
end

execute_instructions(instructions)
print(registers.a)