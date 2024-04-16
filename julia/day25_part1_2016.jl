function parse_instruction(line)
    parts = split(line)
    cmd = parts[1]
    args = parts[2:end]
    return (cmd, args)
end

function execute_program(instructions, initial_registers)
    registers = copy(initial_registers)  # Use the initial registers passed as an argument
    pc = 1
    output = []

    function get_value(x)
        if occursin(r"^[a-d]$", x)
            return registers[x[1]]
        else
            return parse(Int, x)
        end
    end

    while pc <= length(instructions)
        cmd, args = instructions[pc]
        if cmd == "cpy"
            registers[args[2][1]] = get_value(args[1])
        elseif cmd == "inc"
            registers[args[1][1]] += 1
        elseif cmd == "dec"
            registers[args[1][1]] -= 1
        elseif cmd == "jnz"
            if get_value(args[1]) != 0
                pc += get_value(args[2]) - 1
            end
        elseif cmd == "out"
            push!(output, get_value(args[1]))
            if length(output) >= 10
                # Check if the output alternates properly
                if all(output[i] != output[i+1] for i in 1:length(output)-1)
                    return true, output
                else
                    return false, output
                end
            end
        end
        pc += 1
    end
    return false, output
end

function find_minimum_a(filename)
    instructions = []
    open(filename, "r") do file
        for line in eachline(file)
            push!(instructions, parse_instruction(line))
        end
    end

    a = 0
    while true
        a += 1
        initial_registers = Dict('a' => a, 'b' => 0, 'c' => 0, 'd' => 0)
        success, output = execute_program(instructions, initial_registers)
        if success
            return a
        end
    end
end

# Main execution
filename = "input.txt"
result = find_minimum_a(filename)
println("The smallest positive integer for register 'a' is: ", result)