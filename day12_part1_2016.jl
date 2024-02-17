
registers = Dict("a" => 0, "b" => 0, "c" => 0, "d" => 0)

function executeInstructions(instructions, registers)
    i = 1
    while i <= length(instructions)
        parts = split(instructions[i])
        if parts[1] == "cpy"
            val = getValue(parts[2], registers)
            registers[parts[3]] = val
            i += 1
        elseif parts[1] == "inc"
            registers[parts[2]] += 1
            i += 1
        elseif parts[1] == "dec"
            registers[parts[2]] -= 1
            i += 1
        elseif parts[1] == "jnz"
            val = getValue(parts[2], registers)
            if val != 0
                jump = parse(Int, parts[3])
                i += jump
            else
                i += 1
            end
        end
    end
end

function getValue(s, registers)
    val = tryparse(Int, s)
    if val == nothing
        return registers[s]
    end
    return val
end

instructions = readlines("input.txt")
executeInstructions(instructions, registers)

println(registers["a"])
