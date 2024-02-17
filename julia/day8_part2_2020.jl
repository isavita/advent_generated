
function parseInstruction(instruction)
    parts = split(instruction)
    op = parts[1]
    arg = parse(Int, parts[2])
    return op, arg
end

function executeBootCode(instructions)
    accumulator = 0
    visited = Dict{Int, Bool}()
    currentInstruction = 1

    while currentInstruction <= length(instructions)
        if haskey(visited, currentInstruction)
            return accumulator, false
        end

        visited[currentInstruction] = true
        op, arg = parseInstruction(instructions[currentInstruction])

        if op == "acc"
            accumulator += arg
            currentInstruction += 1
        elseif op == "jmp"
            currentInstruction += arg
        else
            currentInstruction += 1
        end
    end

    return accumulator, true
end

instructions = readlines("input.txt")
for i in 1:length(instructions)
    op, arg = parseInstruction(instructions[i])
    if op == "acc"
        continue
    end

    modifiedInstructions = copy(instructions)
    if op == "jmp"
        modifiedInstructions[i] = "nop $arg"
    else
        modifiedInstructions[i] = "jmp $arg"
    end

    accumulator, terminated = executeBootCode(modifiedInstructions)
    if terminated
        println(accumulator)
        break
    end
end
