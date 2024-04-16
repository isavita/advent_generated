function getMode(instruction, position)
    return math.floor(instruction / (10 ^ (position + 1))) % 10
end

function getParam(program, pointer, mode)
    if mode == 0 then
        return program[program[pointer] + 1]
    end
    return program[pointer]
end

function runProgram(program, input)
    local output = 0
    local pointer = 1
    while pointer <= #program do
        local instruction = program[pointer]
        local opcode = instruction % 100

        if opcode == 1 or opcode == 2 then
            local param1 = getParam(program, pointer + 1, getMode(instruction, 1))
            local param2 = getParam(program, pointer + 2, getMode(instruction, 2))
            local result = 0
            if opcode == 1 then
                result = param1 + param2
            else
                result = param1 * param2
            end
            program[program[pointer + 3] + 1] = result
            pointer = pointer + 4
        elseif opcode == 3 then
            program[program[pointer + 1] + 1] = input
            pointer = pointer + 2
        elseif opcode == 4 then
            output = getParam(program, pointer + 1, getMode(instruction, 1))
            pointer = pointer + 2
        elseif opcode == 99 then
            return output
        else
            error("Unknown opcode: " .. opcode)
        end
    end
    return output
end

function main()
    local file = io.open("input.txt", "r")
    local data = file:read("*all")
    file:close()
    local strProgram = {}
    for str in string.gmatch(data, "[^,]+") do
        table.insert(strProgram, tonumber(str))
    end
    local program = {}
    for i, v in ipairs(strProgram) do
        program[i] = v
    end
    print(runProgram(program, 1))
end

main()