function parse_instruction(line)
    parts = split(line)
    return (parts[1], parts[2:end])
end

function run_assembunny(instructions)
    pc = 1
    registers = Dict('a' => 7, 'b' => 0, 'c' => 0, 'd' => 0)  # Initialize 'a' to 7 as per problem statement
    
    function get_value(x)
        if occursin(r"^-?\d+$", x)
            return parse(Int, x)
        else
            return registers[x[1]]
        end
    end
    
    while pc <= length(instructions)
        inst, args = instructions[pc]
        if inst == "cpy" && length(args) == 2 && isletter(args[2][1])
            registers[args[2][1]] = get_value(args[1])
        elseif inst == "inc" && length(args) == 1 && isletter(args[1][1])
            registers[args[1][1]] += 1
        elseif inst == "dec" && length(args) == 1 && isletter(args[1][1])
            registers[args[1][1]] -= 1
        elseif inst == "jnz" && length(args) == 2 && get_value(args[1]) != 0
            pc += get_value(args[2]) - 1
        elseif inst == "tgl" && length(args) == 1
            target = pc + get_value(args[1])
            if 1 <= target <= length(instructions)
                tgt_inst, tgt_args = instructions[target]
                if length(tgt_args) == 1
                    instructions[target] = (tgt_inst == "inc" ? "dec" : "inc", tgt_args)
                elseif length(tgt_args) == 2
                    instructions[target] = (tgt_inst == "jnz" ? "cpy" : "jnz", tgt_args)
                end
            end
        end
        pc += 1
    end
    
    return registers['a']
end

function main()
    instructions = []
    for line in readlines("input.txt")
        push!(instructions, parse_instruction(line))
    end
    result = run_assembunny(instructions)
    println(result)
end

main()