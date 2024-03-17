-- Read input from "input.txt"
local file = io.open("input.txt", "r")
local content = file:read("*a")
file:close()

-- Parse the input
local lines = {}
for line in content:gmatch("[^\r\n]+") do
    table.insert(lines, line)
end

local ip_bind = tonumber(lines[1]:match("#ip (%d+)"))
local instructions = {}
for i = 2, #lines do
    local inst = {}
    for word in lines[i]:gmatch("%w+") do
        table.insert(inst, tonumber(word) or word)
    end
    table.insert(instructions, inst)
end

-- Execute the program
local registers = {0, 0, 0, 0, 0, 0}
local ip = 0
while ip >= 0 and ip < #instructions do
    registers[ip_bind + 1] = ip
    local inst = instructions[ip + 1]
    local opcode, a, b, c = table.unpack(inst)

    if opcode == "addr" then
        registers[c + 1] = registers[a + 1] + registers[b + 1]
    elseif opcode == "addi" then
        registers[c + 1] = registers[a + 1] + b
    elseif opcode == "mulr" then
        registers[c + 1] = registers[a + 1] * registers[b + 1]
    elseif opcode == "muli" then
        registers[c + 1] = registers[a + 1] * b
    elseif opcode == "banr" then
        registers[c + 1] = bit.band(registers[a + 1], registers[b + 1])
    elseif opcode == "bani" then
        registers[c + 1] = bit.band(registers[a + 1], b)
    elseif opcode == "borr" then
        registers[c + 1] = bit.bor(registers[a + 1], registers[b + 1])
    elseif opcode == "bori" then
        registers[c + 1] = bit.bor(registers[a + 1], b)
    elseif opcode == "setr" then
        registers[c + 1] = registers[a + 1]
    elseif opcode == "seti" then
        registers[c + 1] = a
    elseif opcode == "gtir" then
        registers[c + 1] = a > registers[b + 1] and 1 or 0
    elseif opcode == "gtri" then
        registers[c + 1] = registers[a + 1] > b and 1 or 0
    elseif opcode == "gtrr" then
        registers[c + 1] = registers[a + 1] > registers[b + 1] and 1 or 0
    elseif opcode == "eqir" then
        registers[c + 1] = a == registers[b + 1] and 1 or 0
    elseif opcode == "eqri" then
        registers[c + 1] = registers[a + 1] == b and 1 or 0
    elseif opcode == "eqrr" then
        registers[c + 1] = registers[a + 1] == registers[b + 1] and 1 or 0
    end

    ip = registers[ip_bind + 1] + 1
end

print(registers[1])