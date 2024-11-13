
-- Read Intcode program from input file
local function readProgram(filename)
    local file = io.open(filename, "r")
    local program = {}
    if file then
        local content = file:read("*all")
        for num in content:gmatch("[^,]+") do
            table.insert(program, tonumber(num))
        end
        file:close()
    end
    return program
end

-- Deep copy of table
local function deepCopy(orig)
    local copy = {}
    for k, v in pairs(orig) do
        if type(v) == 'table' then
            copy[k] = deepCopy(v)
        else
            copy[k] = v
        end
    end
    return copy
end

-- Intcode computer
local function runIntcode(program, inputs)
    local memory = deepCopy(program)
    local inputIndex = 1
    local pc = 0
    local relativeBase = 0

    local function getParam(mode, param)
        mode = mode or 0
        if mode == 0 then
            return memory[param + 1] or 0
        elseif mode == 1 then
            return param
        elseif mode == 2 then
            return memory[relativeBase + param + 1] or 0
        end
    end

    local function setParam(mode, param, value)
        mode = mode or 0
        if mode == 0 then
            memory[param + 1] = value
        elseif mode == 2 then
            memory[relativeBase + param + 1] = value
        end
    end

    while true do
        local instruction = memory[pc + 1]
        local opcode = instruction % 100
        local modes = {
            math.floor(instruction / 100) % 10,
            math.floor(instruction / 1000) % 10,
            math.floor(instruction / 10000) % 10
        }

        if opcode == 99 then
            break
        elseif opcode == 3 then
            setParam(modes[1], memory[pc + 2], inputs[inputIndex])
            inputIndex = inputIndex + 1
            pc = pc + 2
        elseif opcode == 4 then
            local output = getParam(modes[1], memory[pc + 2])
            pc = pc + 2
            return output
        elseif opcode == 1 then
            local a = getParam(modes[1], memory[pc + 2])
            local b = getParam(modes[2], memory[pc + 3])
            setParam(modes[3], memory[pc + 4], a + b)
            pc = pc + 4
        elseif opcode == 2 then
            local a = getParam(modes[1], memory[pc + 2])
            local b = getParam(modes[2], memory[pc + 3])
            setParam(modes[3], memory[pc + 4], a * b)
            pc = pc + 4
        elseif opcode == 5 then
            local a = getParam(modes[1], memory[pc + 2])
            local b = getParam(modes[2], memory[pc + 3])
            pc = a ~= 0 and b or pc + 3
        elseif opcode == 6 then
            local a = getParam(modes[1], memory[pc + 2])
            local b = getParam(modes[2], memory[pc + 3])
            pc = a == 0 and b or pc + 3
        elseif opcode == 7 then
            local a = getParam(modes[1], memory[pc + 2])
            local b = getParam(modes[2], memory[pc + 3])
            setParam(modes[3], memory[pc + 4], a < b and 1 or 0)
            pc = pc + 4
        elseif opcode == 8 then
            local a = getParam(modes[1], memory[pc + 2])
            local b = getParam(modes[2], memory[pc + 3])
            setParam(modes[3], memory[pc + 4], a == b and 1 or 0)
            pc = pc + 4
        elseif opcode == 9 then
            relativeBase = relativeBase + getParam(modes[1], memory[pc + 2])
            pc = pc + 2
        end
    end
    return nil
end

-- Check if point is in tractor beam
local function isInTractorBeam(program, x, y)
    local programCopy = deepCopy(program)
    return runIntcode(programCopy, {x, y}) == 1
end

-- Count points in tractor beam
local function countTractorBeamPoints(program, gridSize)
    local count = 0
    for y = 0, gridSize - 1 do
        for x = 0, gridSize - 1 do
            if isInTractorBeam(program, x, y) then
                count = count + 1
            end
        end
    end
    return count
end

-- Main program
local program = readProgram("input.txt")
local result = countTractorBeamPoints(program, 50)
print("Points in tractor beam:", result)
