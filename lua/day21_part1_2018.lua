local opcodeNamesToFuncs = {
    addr = function(registers, abcValues) registers[abcValues[3]+1] = registers[abcValues[1]+1] + registers[abcValues[2]+1] return registers end,
    addi = function(registers, abcValues) registers[abcValues[3]+1] = registers[abcValues[1]+1] + abcValues[2] return registers end,
    mulr = function(registers, abcValues) registers[abcValues[3]+1] = registers[abcValues[1]+1] * registers[abcValues[2]+1] return registers end,
    muli = function(registers, abcValues) registers[abcValues[3]+1] = registers[abcValues[1]+1] * abcValues[2] return registers end,
    banr = function(registers, abcValues) registers[abcValues[3]+1] = registers[abcValues[1]+1] & registers[abcValues[2]+1] return registers end,
    bani = function(registers, abcValues) registers[abcValues[3]+1] = registers[abcValues[1]+1] & abcValues[2] return registers end,
    borr = function(registers, abcValues) registers[abcValues[3]+1] = registers[abcValues[1]+1] | registers[abcValues[2]+1] return registers end,
    bori = function(registers, abcValues) registers[abcValues[3]+1] = registers[abcValues[1]+1] | abcValues[2] return registers end,
    setr = function(registers, abcValues) registers[abcValues[3]+1] = registers[abcValues[1]+1] return registers end,
    seti = function(registers, abcValues) registers[abcValues[3]+1] = abcValues[1] return registers end,
    gtir = function(registers, abcValues) registers[abcValues[3]+1] = abcValues[1] > registers[abcValues[2]+1] and 1 or 0 return registers end,
    gtri = function(registers, abcValues) registers[abcValues[3]+1] = registers[abcValues[1]+1] > abcValues[2] and 1 or 0 return registers end,
    gtrr = function(registers, abcValues) registers[abcValues[3]+1] = registers[abcValues[1]+1] > registers[abcValues[2]+1] and 1 or 0 return registers end,
    eqir = function(registers, abcValues) registers[abcValues[3]+1] = abcValues[1] == registers[abcValues[2]+1] and 1 or 0 return registers end,
    eqri = function(registers, abcValues) registers[abcValues[3]+1] = registers[abcValues[1]+1] == abcValues[2] and 1 or 0 return registers end,
    eqrr = function(registers, abcValues) registers[abcValues[3]+1] = registers[abcValues[1]+1] == registers[abcValues[2]+1] and 1 or 0 return registers end
}

local function tick(opcodeComputer)
    if opcodeComputer.registers[opcodeComputer.instructionPointer+1] >= #opcodeComputer.instructions then
        print("Out of range instruction, terminating...")
        return true
    end

    local instIndex = opcodeComputer.registers[opcodeComputer.instructionPointer+1]
    local inst = opcodeComputer.instructions[instIndex+1]

    local opcodeFunc = opcodeNamesToFuncs[inst.name]

    opcodeComputer.registers = opcodeFunc(opcodeComputer.registers, inst.abcValues)

    opcodeComputer.registers[opcodeComputer.instructionPointer+1] = opcodeComputer.registers[opcodeComputer.instructionPointer+1] + 1

    if opcodeComputer.registers[opcodeComputer.instructionPointer+1] >= #opcodeComputer.instructions then
        return true
    end

    return false
end

local function parseInput(input)
    local lines = {}
    for line in input:gmatch("[^\r\n]+") do
        table.insert(lines, line)
    end

    local instructionPointer = tonumber(lines[1]:match("#ip (%d+)"))
    local instructions = {}

    for i = 2, #lines do
        local name, a, b, c = lines[i]:match("(%a+) (%d+) (%d+) (%d+)")
        table.insert(instructions, {name = name, abcValues = {tonumber(a), tonumber(b), tonumber(c)}})
    end

    return {instructions = instructions, registers = {0, 0, 0, 0, 0, 0}, instructionPointer = instructionPointer}
end

local function solve(opcodeComputer)
    while not tick(opcodeComputer) do
        if opcodeComputer.registers[opcodeComputer.instructionPointer+1] == 28 then
            break
        end
    end

    return opcodeComputer.registers[6]
end

local file = io.open("input.txt", "r")
if file then
    local input = file:read("*all")
    file:close()

    local opcodeComputer = parseInput(input)
    print(solve(opcodeComputer))
else
    print("Error: Unable to open file")
end