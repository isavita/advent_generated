
local file = io.open("input.txt", "r")
local instructions = {}
for line in file:lines() do
    table.insert(instructions, line)
end
file:close()

local registers = {a = 7, b = 0, c = 0, d = 0}

local function getValue(s, registers)
    local val = tonumber(s)
    if val then
        return val
    else
        return registers[s]
    end
end

local function toggleInstruction(instr)
    local parts = {}
    for part in string.gmatch(instr, "%S+") do
        table.insert(parts, part)
    end
    if parts[1] == "inc" then
        parts[1] = "dec"
    elseif parts[1] == "dec" or parts[1] == "tgl" then
        parts[1] = "inc"
    elseif parts[1] == "jnz" then
        parts[1] = "cpy"
    elseif parts[1] == "cpy" then
        parts[1] = "jnz"
    end
    return table.concat(parts, " ")
end

local pc = 1
while pc <= #instructions do
    local fields = {}
    for field in string.gmatch(instructions[pc], "%S+") do
        table.insert(fields, field)
    end
    local instruction = fields[1]
    if instruction == "cpy" then
        local x = getValue(fields[2], registers)
        if registers[fields[3]] then
            registers[fields[3]] = x
        end
    elseif instruction == "inc" then
        if registers[fields[2]] then
            registers[fields[2]] = registers[fields[2]] + 1
        end
    elseif instruction == "dec" then
        if registers[fields[2]] then
            registers[fields[2]] = registers[fields[2]] - 1
        end
    elseif instruction == "jnz" then
        local x = getValue(fields[2], registers)
        if x ~= 0 then
            pc = pc + getValue(fields[3], registers) - 1
        end
    elseif instruction == "tgl" then
        local x = getValue(fields[2], registers)
        local tgt = pc + x
        if tgt >= 1 and tgt <= #instructions then
            instructions[tgt] = toggleInstruction(instructions[tgt])
        end
    end
    pc = pc + 1
end

print(registers["a"])
