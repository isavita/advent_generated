
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
            return accumulator, true
        end

        visited[currentInstruction] = true
        local parts = {}
        for part in string.gmatch(instructions[currentInstruction], "%S+") do
            table.insert(parts, part)
        end
        local op = parts[1]
        local arg = tonumber(parts[2])

        if op == "acc" then
            accumulator = accumulator + arg
            currentInstruction = currentInstruction + 1
        elseif op == "jmp" then
            currentInstruction = currentInstruction + arg
        else
            currentInstruction = currentInstruction + 1
        end
    end

    return accumulator, false
end

local accumulator, _ = executeBootCode(instructions)
print(accumulator)
