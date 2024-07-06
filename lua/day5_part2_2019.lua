-- Read input from file
local function read_input()
    local file = io.open("input.txt", "r")
    if not file then error("Could not open input file") end
    local content = file:read("*all")
    file:close()
    local program = {}
    for num in content:gmatch("[-]?%d+") do
        table.insert(program, tonumber(num))
    end
    return program
end

-- Get parameter value based on mode
local function get_param(program, pos, mode)
    if mode == 0 then  -- position mode
        return program[program[pos] + 1]
    else  -- immediate mode
        return program[pos]
    end
end

-- Intcode computer
local function intcode_computer(program, input)
    local pos = 1
    local output = {}

    while true do
        local instruction = program[pos]
        local opcode = instruction % 100
        local mode1 = math.floor(instruction / 100) % 10
        local mode2 = math.floor(instruction / 1000) % 10
        local mode3 = math.floor(instruction / 10000) % 10

        if opcode == 99 then  -- halt
            break
        elseif opcode == 1 then  -- add
            local p1 = get_param(program, pos + 1, mode1)
            local p2 = get_param(program, pos + 2, mode2)
            program[program[pos + 3] + 1] = p1 + p2
            pos = pos + 4
        elseif opcode == 2 then  -- multiply
            local p1 = get_param(program, pos + 1, mode1)
            local p2 = get_param(program, pos + 2, mode2)
            program[program[pos + 3] + 1] = p1 * p2
            pos = pos + 4
        elseif opcode == 3 then  -- input
            program[program[pos + 1] + 1] = input
            pos = pos + 2
        elseif opcode == 4 then  -- output
            local p1 = get_param(program, pos + 1, mode1)
            table.insert(output, p1)
            pos = pos + 2
        elseif opcode == 5 then  -- jump-if-true
            local p1 = get_param(program, pos + 1, mode1)
            local p2 = get_param(program, pos + 2, mode2)
            if p1 ~= 0 then
                pos = p2 + 1
            else
                pos = pos + 3
            end
        elseif opcode == 6 then  -- jump-if-false
            local p1 = get_param(program, pos + 1, mode1)
            local p2 = get_param(program, pos + 2, mode2)
            if p1 == 0 then
                pos = p2 + 1
            else
                pos = pos + 3
            end
        elseif opcode == 7 then  -- less than
            local p1 = get_param(program, pos + 1, mode1)
            local p2 = get_param(program, pos + 2, mode2)
            program[program[pos + 3] + 1] = p1 < p2 and 1 or 0
            pos = pos + 4
        elseif opcode == 8 then  -- equals
            local p1 = get_param(program, pos + 1, mode1)
            local p2 = get_param(program, pos + 2, mode2)
            program[program[pos + 3] + 1] = p1 == p2 and 1 or 0
            pos = pos + 4
        else
            error("Unknown opcode: " .. opcode)
        end
    end

    return output
end

-- Main function
local function main()
    local program = read_input()
    local output = intcode_computer(program, 5)
    print("Diagnostic code for system ID 5:", output[#output])
end

main()
