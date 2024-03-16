using Printf

function main()
    instructions = readlines("input.txt")

    # Initialize the registers
    registers = Dict("a" => 0, "b" => 0)

    # Execute the instructions
    i = 1
    while i <= length(instructions)
        parts = split(instructions[i])

        if parts[1] == "hlf"
            registers[parts[2]] ÷= 2
        elseif parts[1] == "tpl"
            registers[parts[2]] *= 3
        elseif parts[1] == "inc"
            registers[parts[2]] += 1
        elseif parts[1] == "jmp"
            i += parse(Int, parts[2]) - 1
        elseif parts[1] == "jie"
            if registers[parts[2][1:1]] % 2 == 0
                i += parse(Int, parts[3]) - 1
            end
        elseif parts[1] == "jio"
            if registers[parts[2][1:1]] == 1
                i += parse(Int, parts[3]) - 1
            end
        else
            error("Unknown instruction: $(parts[1])")
        end

        i += 1
    end

    @printf("%d\n", registers["b"])
end

main()