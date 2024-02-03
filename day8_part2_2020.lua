
local file = io.open("input.txt", "r")
local instructions = {}
for line in file:lines() do
    table.insert(instructions, line)
end
file:close()

function executeBootCode(instructions)
    local accumulator = 0
    local visited = {}
    local currentInstruction = 1

    while currentInstruction <= #instructions do
        if visited[currentInstruction] then
            return accumulator, false
        end

        visited[currentInstruction] = true
        local op, arg = parseInstruction(instructions[currentInstruction])

        if op == "acc" then
            accumulator = accumulator + arg
            currentInstruction = currentInstruction + 1
        elseif op == "jmp" then
            currentInstruction = currentInstruction + arg
        else
            currentInstruction = currentInstruction + 1
        end
    end

    return accumulator, true
end

function parseInstruction(instruction)
    local parts = {}
    for part in string.gmatch(instruction, "%S+") do
        table.insert(parts, part)
    end
    local op = parts[1]
    local arg = tonumber(parts[2])
    return op, arg
end

for i = 1, #instructions do
    local op, arg = parseInstruction(instructions[i])
    if op == "acc" then
        goto continue
    end

    local modifiedInstructions = {}
    for j = 1, #instructions do
        modifiedInstructions[j] = instructions[j]
    end

    if op == "jmp" then
        modifiedInstructions[i] = string.format("nop %d", arg)
    else
        modifiedInstructions[i] = string.format("jmp %d", arg)
    end

    local accumulator, terminated = executeBootCode(modifiedInstructions)
    if terminated then
        print(accumulator)
        break
    end

    ::continue::
end
