
local file = io.open("input.txt", "r")
local instructions = {}
for line in file:lines() do
    table.insert(instructions, line)
end
file:close()

local registers = {a = 0, b = 0, c = 0, d = 0}

local function executeInstructions(instructions, registers)
    local i = 1
    while i <= #instructions do
        local parts = {}
        for part in string.gmatch(instructions[i], "%S+") do
            table.insert(parts, part)
        end
        if parts[1] == "cpy" then
            local val = tonumber(parts[2]) or registers[parts[2]]
            registers[parts[3]] = val
            i = i + 1
        elseif parts[1] == "inc" then
            registers[parts[2]] = registers[parts[2]] + 1
            i = i + 1
        elseif parts[1] == "dec" then
            registers[parts[2]] = registers[parts[2]] - 1
            i = i + 1
        elseif parts[1] == "jnz" then
            local val = tonumber(parts[2]) or registers[parts[2]]
            if val ~= 0 then
                local jump = tonumber(parts[3])
                i = i + jump
            else
                i = i + 1
            end
        end
    end
end

executeInstructions(instructions, registers)
print(registers["a"])
