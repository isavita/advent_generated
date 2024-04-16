function parse_value(value, registers)
    if occursin(r"^-?\d+$", value)  # Check if the value is a number
        return parse(Int, value)
    else
        return get(registers, value, 0)  # Return the value in the register, default to 0
    end
end

function execute_instructions(filename)
    registers = Dict{String, Int}()
    instructions = readlines(filename)
    last_sound = 0
    pc = 1  # Program counter

    while 1 <= pc <= length(instructions)
        parts = split(instructions[pc])
        instruction = parts[1]
        reg = parts[2]

        val = length(parts) > 2 ? parse_value(parts[3], registers) : 0

        if instruction == "snd"
            last_sound = parse_value(reg, registers)
        elseif instruction == "set"
            registers[reg] = val
        elseif instruction == "add"
            registers[reg] = get(registers, reg, 0) + val
        elseif instruction == "mul"
            registers[reg] = get(registers, reg, 0) * val
        elseif instruction == "mod"
            registers[reg] = get(registers, reg, 0) % val
        elseif instruction == "rcv"
            if parse_value(reg, registers) != 0
                return last_sound
            end
        elseif instruction == "jgz"
            if parse_value(reg, registers) > 0
                pc += val
                continue
            end
        end

        pc += 1
    end

    return -1  # If the program completes without executing a valid rcv
end

# Read from input.txt and print the recovered frequency
println("Recovered frequency: ", execute_instructions("input.txt"))