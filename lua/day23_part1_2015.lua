-- Read the input from the "input.txt" file
local file = io.open("input.txt", "r")
local instructions = {}
for line in file:lines() do
    table.insert(instructions, line)
end
file:close()

-- Initialize the registers
local registers = {a = 0, b = 0}

-- Execute the instructions
local i = 1
while i <= #instructions do
    local parts = {}
    for part in string.gmatch(instructions[i], "%S+") do
        table.insert(parts, part)
    end

    if parts[1] == "hlf" then
        registers[parts[2]] = math.floor(registers[parts[2]] / 2)
    elseif parts[1] == "tpl" then
        registers[parts[2]] = registers[parts[2]] * 3
    elseif parts[1] == "inc" then
        registers[parts[2]] = registers[parts[2]] + 1
    elseif parts[1] == "jmp" then
        i = i + tonumber(parts[2]) - 1
    elseif parts[1] == "jie" then
        if registers[string.sub(parts[2], 1, 1)] % 2 == 0 then
            i = i + tonumber(parts[3]) - 1
        end
    elseif parts[1] == "jio" then
        if registers[string.sub(parts[2], 1, 1)] == 1 then
            i = i + tonumber(parts[3]) - 1
        end
    else
        error("Unknown instruction: " .. parts[1])
    end

    i = i + 1
end

print(registers.b)