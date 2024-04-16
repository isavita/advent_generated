function run_instructions(filename, initial_a)
    # Initialize registers
    registers = Dict('a' => initial_a, 'b' => 0)

    # Read instructions from file
    instructions = readlines(filename)

    # Current position of instruction
    position = 1

    # Process each instruction
    while position > 0 && position <= length(instructions)
        parts = split(instructions[position])
        instr = parts[1]

        # Execute based on instruction type
        if instr == "hlf"
            registers[parts[2][1]] ÷= 2
            position += 1
        elseif instr == "tpl"
            registers[parts[2][1]] *= 3
            position += 1
        elseif instr == "inc"
            registers[parts[2][1]] += 1
            position += 1
        elseif instr == "jmp"
            position += parse(Int, parts[2])
        elseif instr == "jie"
            if registers[parts[2][1]] % 2 == 0
                position += parse(Int, parts[3])
            else
                position += 1
            end
        elseif instr == "jio"
            if registers[parts[2][1]] == 1
                position += parse(Int, parts[3])
            else
                position += 1
            end
        end
    end

    # Return the value in register b
    registers['b']
end

# Solve part 1
println("Part 1: Register b = ", run_instructions("input.txt", 0))

# Solve part 2
println("Part 2: Register b = ", run_instructions("input.txt", 1))