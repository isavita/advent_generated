
instructions = readlines("input.txt")

function executeBootCode(instructions)
    accumulator = 0
    visited = Dict()
    currentInstruction = 1

    while currentInstruction <= length(instructions)
        if haskey(visited, currentInstruction)
            return accumulator, true
        end

        visited[currentInstruction] = true
        parts = split(instructions[currentInstruction])
        op = parts[1]
        arg = parse(Int, parts[2])

        if op == "acc"
            accumulator += arg
            currentInstruction += 1
        elseif op == "jmp"
            currentInstruction += arg
        elseif op == "nop"
            currentInstruction += 1
        end
    end

    return accumulator, false
end

accumulator, _ = executeBootCode(instructions)
println(accumulator)
