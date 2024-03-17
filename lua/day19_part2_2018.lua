local file = io.open("input.txt", "r")
local lines = {}
for line in file:lines() do
    table.insert(lines, line)
end
file:close()

local instructions = {
    ["addr"] = function(r, a, b) return r[a+1] + r[b+1] end,
    ["addi"] = function(r, a, b) return r[a+1] + b end,
    ["mulr"] = function(r, a, b) return r[a+1] * r[b+1] end,
    ["muli"] = function(r, a, b) return r[a+1] * b end,
    ["banr"] = function(r, a, b) return bit.band(r[a+1], r[b+1]) end,
    ["bani"] = function(r, a, b) return bit.band(r[a+1], b) end,
    ["borr"] = function(r, a, b) return bit.bor(r[a+1], r[b+1]) end,
    ["bori"] = function(r, a, b) return bit.bor(r[a+1], b) end,
    ["setr"] = function(r, a, b) return r[a+1] end,
    ["seti"] = function(r, a, b) return a end,
    ["gtir"] = function(r, a, b) return (a > r[b+1]) and 1 or 0 end,
    ["gtri"] = function(r, a, b) return (r[a+1] > b) and 1 or 0 end,
    ["gtrr"] = function(r, a, b) return (r[a+1] > r[b+1]) and 1 or 0 end,
    ["eqir"] = function(r, a, b) return (a == r[b+1]) and 1 or 0 end,
    ["eqri"] = function(r, a, b) return (r[a+1] == b) and 1 or 0 end,
    ["eqrr"] = function(r, a, b) return (r[a+1] == r[b+1]) and 1 or 0 end
}

local function loadProgram(lines)
    local ipRegister = tonumber(string.match(lines[1], "#ip (%d+)"))
    local program = {}
    for i = 2, #lines do
        local parts = {}
        for part in string.gmatch(lines[i], "%S+") do
            table.insert(parts, part)
        end
        local op = instructions[parts[1]]
        local a = tonumber(parts[2])
        local b = tonumber(parts[3])
        local c = tonumber(parts[4])
        table.insert(program, function(r) r[c+1] = op(r, a, b) end)
    end
    return ipRegister, program
end

local function runProgram(ipRegister, program, registers, maxCycles)
    local ip = 0
    local cycles = 0
    while ip >= 0 and ip < #program do
        registers[ipRegister+1] = ip
        program[ip+1](registers)
        ip = registers[ipRegister+1] + 1
        cycles = cycles + 1
        if maxCycles > 0 and cycles >= maxCycles then
            break
        end
    end
    return registers
end

local function max(slice)
    local maxValue = slice[1]
    for i = 2, #slice do
        if slice[i] > maxValue then
            maxValue = slice[i]
        end
    end
    return maxValue
end

local ipRegister, program = loadProgram(lines)
local registers = {1, 0, 0, 0, 0, 0}
registers = runProgram(ipRegister, program, registers, 1000)
local n = max(registers)
local total = 0
for i = 1, n do
    if n % i == 0 then
        total = total + i
    end
end
print(total)