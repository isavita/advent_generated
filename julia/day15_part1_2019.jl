
using DataStructures

# Intcode computer implementation
mutable struct IntcodeComputer
    memory::Vector{Int}
    ip::Int
    relative_base::Int
    inputs::Vector{Int}
    outputs::Vector{Int}
    halted::Bool

    function IntcodeComputer(program::Vector{Int})
        memory = copy(program)
        resize!(memory, length(memory) + 10000)
        new(memory, 1, 0, [], [], false)
    end
end

function get_param(computer::IntcodeComputer, mode::Int, param::Int)
    if mode == 0  # position mode
        return computer.memory[param + 1]
    elseif mode == 1  # immediate mode
        return param
    elseif mode == 2  # relative mode
        return computer.memory[param + computer.relative_base + 1]
    end
end

function set_param(computer::IntcodeComputer, mode::Int, param::Int, value::Int)
    if mode == 0  # position mode
        computer.memory[param + 1] = value
    elseif mode == 2  # relative mode
        computer.memory[param + computer.relative_base + 1] = value
    end
end

function run_intcode!(computer::IntcodeComputer)
    while true
        instruction = computer.memory[computer.ip]
        opcode = instruction % 100
        modes = div(instruction, 100)

        if opcode == 99
            computer.halted = true
            break
        end

        mode1 = modes % 10
        mode2 = div(modes, 10) % 10
        mode3 = div(modes, 100) % 10

        if opcode == 1  # addition
            param1 = get_param(computer, mode1, computer.memory[computer.ip + 1])
            param2 = get_param(computer, mode2, computer.memory[computer.ip + 2])
            set_param(computer, mode3, computer.memory[computer.ip + 3], param1 + param2)
            computer.ip += 4
        elseif opcode == 2  # multiplication
            param1 = get_param(computer, mode1, computer.memory[computer.ip + 1])
            param2 = get_param(computer, mode2, computer.memory[computer.ip + 2])
            set_param(computer, mode3, computer.memory[computer.ip + 3], param1 * param2)
            computer.ip += 4
        elseif opcode == 3  # input
            if isempty(computer.inputs)
                break
            end
            input = popfirst!(computer.inputs)
            set_param(computer, mode1, computer.memory[computer.ip + 1], input)
            computer.ip += 2
        elseif opcode == 4  # output
            output = get_param(computer, mode1, computer.memory[computer.ip + 1])
            push!(computer.outputs, output)
            computer.ip += 2
            break
        elseif opcode == 5  # jump-if-true
            param1 = get_param(computer, mode1, computer.memory[computer.ip + 1])
            param2 = get_param(computer, mode2, computer.memory[computer.ip + 2])
            computer.ip = param1 != 0 ? param2 + 1 : computer.ip + 3
        elseif opcode == 6  # jump-if-false
            param1 = get_param(computer, mode1, computer.memory[computer.ip + 1])
            param2 = get_param(computer, mode2, computer.memory[computer.ip + 2])
            computer.ip = param1 == 0 ? param2 + 1 : computer.ip + 3
        elseif opcode == 7  # less than
            param1 = get_param(computer, mode1, computer.memory[computer.ip + 1])
            param2 = get_param(computer, mode2, computer.memory[computer.ip + 2])
            set_param(computer, mode3, computer.memory[computer.ip + 3], param1 < param2 ? 1 : 0)
            computer.ip += 4
        elseif opcode == 8  # equals
            param1 = get_param(computer, mode1, computer.memory[computer.ip + 1])
            param2 = get_param(computer, mode2, computer.memory[computer.ip + 2])
            set_param(computer, mode3, computer.memory[computer.ip + 3], param1 == param2 ? 1 : 0)
            computer.ip += 4
        elseif opcode == 9  # adjust relative base
            param1 = get_param(computer, mode1, computer.memory[computer.ip + 1])
            computer.relative_base += param1
            computer.ip += 2
        end
    end
end

# Solve oxygen system challenge
function solve_oxygen_system(program::Vector{Int})
    directions = [(0, 1), (0, -1), (-1, 0), (1, 0)]
    start = (0, 0)
    visited = Set{Tuple{Int, Int}}([start])
    queue = Queue{Tuple{IntcodeComputer, Tuple{Int, Int}, Int}}()
    
    computer = IntcodeComputer(program)
    enqueue!(queue, (deepcopy(computer), start, 0))
    
    while !isempty(queue)
        current_computer, pos, steps = dequeue!(queue)
        
        for (i, (dx, dy)) in enumerate(directions)
            new_pos = (pos[1] + dx, pos[2] + dy)
            
            if new_pos ∉ visited
                push!(visited, new_pos)
                
                new_computer = deepcopy(current_computer)
                push!(new_computer.inputs, i)
                run_intcode!(new_computer)
                
                status = pop!(new_computer.outputs)
                
                if status == 2  # Oxygen system found
                    return steps + 1
                elseif status == 1  # Moved successfully
                    enqueue!(queue, (new_computer, new_pos, steps + 1))
                end
            end
        end
    end
    
    return -1  # Oxygen system not found
end

# Main program
function main()
    # Read Intcode program from input file
    program = parse.(Int, split(read("input.txt", String), ","))
    
    # Solve the challenge
    result = solve_oxygen_system(program)
    
    # Print the result
    println("Fewest movement commands to oxygen system: ", result)
end

main()
