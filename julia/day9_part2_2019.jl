
function run_intcode(program::Vector{Int}, input::Int)
    # Convert program to a mutable dictionary to support large memory
    memory = Dict{Int, Int}(i-1 => val for (i, val) in enumerate(program))
    
    ip = 0  # instruction pointer
    relative_base = 0
    outputs = Int[]
    
    # Helper function to get parameter value based on mode
    function get_param(mode, param)
        if mode == 0  # position mode
            return get(memory, param, 0)
        elseif mode == 1  # immediate mode
            return param
        elseif mode == 2  # relative mode
            return get(memory, relative_base + param, 0)
        end
    end
    
    # Helper function to get write address based on mode
    function get_write_address(mode, param)
        if mode == 0  # position mode
            return param
        elseif mode == 2  # relative mode
            return relative_base + param
        end
    end
    
    while true
        # Parse instruction
        instruction = get(memory, ip, 0)
        opcode = instruction % 100
        modes = div(instruction, 100)
        
        # Extract parameter modes
        mode1 = modes % 10
        mode2 = div(modes, 10) % 10
        mode3 = div(modes, 100) % 10
        
        if opcode == 99  # halt
            break
        elseif opcode == 1  # addition
            param1 = get_param(mode1, get(memory, ip+1, 0))
            param2 = get_param(mode2, get(memory, ip+2, 0))
            dest = get_write_address(mode3, get(memory, ip+3, 0))
            memory[dest] = param1 + param2
            ip += 4
        elseif opcode == 2  # multiplication
            param1 = get_param(mode1, get(memory, ip+1, 0))
            param2 = get_param(mode2, get(memory, ip+2, 0))
            dest = get_write_address(mode3, get(memory, ip+3, 0))
            memory[dest] = param1 * param2
            ip += 4
        elseif opcode == 3  # input
            dest = get_write_address(mode1, get(memory, ip+1, 0))
            memory[dest] = input
            ip += 2
        elseif opcode == 4  # output
            param = get_param(mode1, get(memory, ip+1, 0))
            push!(outputs, param)
            ip += 2
        elseif opcode == 5  # jump-if-true
            param1 = get_param(mode1, get(memory, ip+1, 0))
            param2 = get_param(mode2, get(memory, ip+2, 0))
            ip = param1 != 0 ? param2 : ip + 3
        elseif opcode == 6  # jump-if-false
            param1 = get_param(mode1, get(memory, ip+1, 0))
            param2 = get_param(mode2, get(memory, ip+2, 0))
            ip = param1 == 0 ? param2 : ip + 3
        elseif opcode == 7  # less than
            param1 = get_param(mode1, get(memory, ip+1, 0))
            param2 = get_param(mode2, get(memory, ip+2, 0))
            dest = get_write_address(mode3, get(memory, ip+3, 0))
            memory[dest] = param1 < param2 ? 1 : 0
            ip += 4
        elseif opcode == 8  # equals
            param1 = get_param(mode1, get(memory, ip+1, 0))
            param2 = get_param(mode2, get(memory, ip+2, 0))
            dest = get_write_address(mode3, get(memory, ip+3, 0))
            memory[dest] = param1 == param2 ? 1 : 0
            ip += 4
        elseif opcode == 9  # adjust relative base
            param = get_param(mode1, get(memory, ip+1, 0))
            relative_base += param
            ip += 2
        else
            error("Invalid opcode: $opcode")
        end
    end
    
    return outputs
end

# Read input from file
function read_input(filename)
    return [parse(Int, x) for x in split(read(filename, String), ",")]
end

# Main function
function main()
    # Read program from input file
    program = read_input("input.txt")
    
    # Part 1: Run in test mode (input 1)
    part1_output = run_intcode(program, 1)
    println("Part 1 output: ", part1_output[end])
    
    # Part 2: Run in sensor boost mode (input 2)
    part2_output = run_intcode(program, 2)
    println("Part 2 output: ", part2_output[end])
end

# Run the program
main()
