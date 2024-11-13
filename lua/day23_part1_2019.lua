
-- Read Intcode program from input file
local function readProgram(filename)
    local file = io.open(filename, "r")
    if not file then error("Could not open file") end
    local program = {}
    for line in file:read("*all"):gmatch("[^,]+") do
        table.insert(program, tonumber(line))
    end
    file:close()
    return program
end

-- Intcode computer implementation
local function createComputer(program, address)
    local memory = {}
    for i, v in ipairs(program) do memory[i-1] = v end
    local ip = 0  -- instruction pointer
    local relativeBase = 0
    local inputQueue = {address}
    local outputBuffer = {}

    local function getParam(mode, param)
        mode = mode or 0
        if mode == 0 then return memory[param] or 0
        elseif mode == 1 then return param
        elseif mode == 2 then return memory[relativeBase + param] or 0
        end
    end

    local function setParam(mode, param, value)
        mode = mode or 0
        if mode == 0 then memory[param] = value
        elseif mode == 2 then memory[relativeBase + param] = value
        end
    end

    local function run()
        while true do
            local instruction = memory[ip]
            local opcode = instruction % 100
            local modes = {
                math.floor(instruction / 100) % 10,
                math.floor(instruction / 1000) % 10,
                math.floor(instruction / 10000) % 10
            }

            if opcode == 99 then break end

            if opcode == 1 then  -- Addition
                local a = getParam(modes[1], memory[ip+1])
                local b = getParam(modes[2], memory[ip+2])
                setParam(modes[3], memory[ip+3], a + b)
                ip = ip + 4
            elseif opcode == 2 then  -- Multiplication
                local a = getParam(modes[1], memory[ip+1])
                local b = getParam(modes[2], memory[ip+2])
                setParam(modes[3], memory[ip+3], a * b)
                ip = ip + 4
            elseif opcode == 3 then  -- Input
                if #inputQueue > 0 then
                    local input = table.remove(inputQueue, 1)
                    setParam(modes[1], memory[ip+1], input)
                    ip = ip + 2
                else
                    setParam(modes[1], memory[ip+1], -1)
                    ip = ip + 2
                    return nil
                end
            elseif opcode == 4 then  -- Output
                local output = getParam(modes[1], memory[ip+1])
                table.insert(outputBuffer, output)
                ip = ip + 2
                if #outputBuffer == 3 then
                    local result = {table.unpack(outputBuffer)}
                    outputBuffer = {}
                    return result
                end
            elseif opcode == 5 then  -- Jump if true
                local a = getParam(modes[1], memory[ip+1])
                local b = getParam(modes[2], memory[ip+2])
                ip = a ~= 0 and b or ip + 3
            elseif opcode == 6 then  -- Jump if false
                local a = getParam(modes[1], memory[ip+1])
                local b = getParam(modes[2], memory[ip+2])
                ip = a == 0 and b or ip + 3
            elseif opcode == 7 then  -- Less than
                local a = getParam(modes[1], memory[ip+1])
                local b = getParam(modes[2], memory[ip+2])
                setParam(modes[3], memory[ip+3], a < b and 1 or 0)
                ip = ip + 4
            elseif opcode == 8 then  -- Equals
                local a = getParam(modes[1], memory[ip+1])
                local b = getParam(modes[2], memory[ip+2])
                setParam(modes[3], memory[ip+3], a == b and 1 or 0)
                ip = ip + 4
            elseif opcode == 9 then  -- Adjust relative base
                relativeBase = relativeBase + getParam(modes[1], memory[ip+1])
                ip = ip + 2
            end
        end
    end

    return {
        run = run,
        inputQueue = inputQueue
    }
end

-- Main network simulation
local function simulateNetwork(program)
    local computers = {}
    local networkQueues = {}

    -- Initialize computers
    for i = 0, 49 do
        computers[i] = createComputer(program, i)
        networkQueues[i] = {}
    end

    while true do
        for addr = 0, 49 do
            local computer = computers[addr]
            local queue = networkQueues[addr]

            -- Add queued packets to input
            for _, packet in ipairs(queue) do
                table.insert(computer.inputQueue, packet[1])
                table.insert(computer.inputQueue, packet[2])
            end
            networkQueues[addr] = {}

            local output = computer.run()
            if output then
                local destAddr, x, y = output[1], output[2], output[3]
                if destAddr == 255 then
                    return y
                end
                table.insert(networkQueues[destAddr], {x, y})
            end
        end
    end
end

-- Main program
local program = readProgram("input.txt")
local result = simulateNetwork(program)
print(result)
