
function parse_input(filename)
    return parse.(Int, split(read(filename, String), ","))
end

function get_param_value(memory, param, mode)
    return mode == 1 ? param : memory[param + 1]
end

function run_intcode(memory, input_value)
    ip = 1  # Instruction pointer starts at position 1
    memory = copy(memory)  # Work on a copy of the memory

    while true
        instruction = memory[ip]
        opcode = instruction % 100
        modes = [(instruction ÷ 10^i) % 10 for i in 2:5]  # Get parameter modes

        if opcode == 99  # Halt
            break
        elseif opcode == 1  # Addition
            param1 = get_param_value(memory, memory[ip + 1], modes[1])
            param2 = get_param_value(memory, memory[ip + 2], modes[2])
            memory[memory[ip + 3] + 1] = param1 + param2
            ip += 4
        elseif opcode == 2  # Multiplication
            param1 = get_param_value(memory, memory[ip + 1], modes[1])
            param2 = get_param_value(memory, memory[ip + 2], modes[2])
            memory[memory[ip + 3] + 1] = param1 * param2
            ip += 4
        elseif opcode == 3  # Input
            memory[memory[ip + 1] + 1] = input_value
            ip += 2
        elseif opcode == 4  # Output
            output_value = get_param_value(memory, memory[ip + 1], modes[1])
            println(output_value)  # Print the output value
            ip += 2
        elseif opcode == 5  # Jump-if-true
            param1 = get_param_value(memory, memory[ip + 1], modes[1])
            if param1 != 0
                ip = get_param_value(memory, memory[ip + 2], modes[2])
            else
                ip += 3
            end
        elseif opcode == 6  # Jump-if-false
            param1 = get_param_value(memory, memory[ip + 1], modes[1])
            if param1 == 0
                ip = get_param_value(memory, memory[ip + 2], modes[2])
            else
                ip += 3
            end
        elseif opcode == 7  # Less than
            param1 = get_param_value(memory, memory[ip + 1], modes[1])
            param2 = get_param_value(memory, memory[ip + 2], modes[2])
            memory[memory[ip + 3] + 1] = param1 < param2 ? 1 : 0
            ip += 4
        elseif opcode == 8  # Equals
            param1 = get_param_value(memory, memory[ip + 1], modes[1])
            param2 = get_param_value(memory, memory[ip + 2], modes[2])
            memory[memory[ip + 3] + 1] = param1 == param2 ? 1 : 0
            ip += 4
        else
            error("Unknown opcode: $opcode")
        end
    end

    return memory
end

function main()
    memory = parse_input("input.txt")
    diagnostic_code_memory = run_intcode(memory, 1)
    # The last output before halt is the diagnostic code
    println("Diagnostic code: ", diagnostic_code_memory[1])  # Assuming the diagnostic code is at position 1
end

main()
