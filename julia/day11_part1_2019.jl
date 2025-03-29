
using Printf

mutable struct Intcode
    memory::Dict{Int, Int}
    ip::Int
    relative_base::Int
    input::Vector{Int}
    output::Vector{Int}
    halted::Bool
end

function Intcode(program::Vector{Int})
    memory = Dict{Int, Int}()
    for (i, v) in enumerate(program)
        memory[i-1] = v # Use 0-based indexing for memory addresses internally
    end
    return Intcode(memory, 0, 0, Int[], Int[], false)
end

function add_input!(vm::Intcode, value::Int)
    push!(vm.input, value)
end

function read_mem(vm::Intcode, address::Int)
    return get(vm.memory, address, 0)
end

function write_mem!(vm::Intcode, address::Int, value::Int)
    vm.memory[address] = value
end

function get_param_address(vm::Intcode, param_num::Int)
    instruction = read_mem(vm, vm.ip)
    mode = (instruction ÷ (10^(1 + param_num))) % 10
    param_val = read_mem(vm, vm.ip + param_num)

    if mode == 0 # Position mode
        return param_val
    elseif mode == 1 # Immediate mode - Invalid for write address
         error("Immediate mode (1) invalid for write parameter")
    elseif mode == 2 # Relative mode
        return vm.relative_base + param_val
    else
        error("Unknown parameter mode: $mode")
    end
end

function get_param_value(vm::Intcode, param_num::Int)
    instruction = read_mem(vm, vm.ip)
    mode = (instruction ÷ (10^(1 + param_num))) % 10
    param_val = read_mem(vm, vm.ip + param_num)

    if mode == 0 # Position mode
        return read_mem(vm, param_val)
    elseif mode == 1 # Immediate mode
        return param_val
    elseif mode == 2 # Relative mode
        return read_mem(vm, vm.relative_base + param_val)
    else
        error("Unknown parameter mode: $mode")
    end
end


function run!(vm::Intcode)
    empty!(vm.output)
    while !vm.halted
        instruction = read_mem(vm, vm.ip)
        opcode = instruction % 100

        if opcode == 1 # Add
            val1 = get_param_value(vm, 1)
            val2 = get_param_value(vm, 2)
            dest_addr = get_param_address(vm, 3)
            write_mem!(vm, dest_addr, val1 + val2)
            vm.ip += 4
        elseif opcode == 2 # Multiply
            val1 = get_param_value(vm, 1)
            val2 = get_param_value(vm, 2)
            dest_addr = get_param_address(vm, 3)
            write_mem!(vm, dest_addr, val1 * val2)
            vm.ip += 4
        elseif opcode == 3 # Input
            if isempty(vm.input)
                return # Paused, waiting for input
            end
            input_val = popfirst!(vm.input)
            dest_addr = get_param_address(vm, 1)
            write_mem!(vm, dest_addr, input_val)
            vm.ip += 2
        elseif opcode == 4 # Output
            output_val = get_param_value(vm, 1)
            push!(vm.output, output_val)
            vm.ip += 2
        elseif opcode == 5 # Jump-if-true
            val = get_param_value(vm, 1)
            target = get_param_value(vm, 2)
            if val != 0
                vm.ip = target
            else
                vm.ip += 3
            end
        elseif opcode == 6 # Jump-if-false
            val = get_param_value(vm, 1)
            target = get_param_value(vm, 2)
            if val == 0
                vm.ip = target
            else
                vm.ip += 3
            end
        elseif opcode == 7 # Less than
            val1 = get_param_value(vm, 1)
            val2 = get_param_value(vm, 2)
            dest_addr = get_param_address(vm, 3)
            write_mem!(vm, dest_addr, val1 < val2 ? 1 : 0)
            vm.ip += 4
        elseif opcode == 8 # Equals
            val1 = get_param_value(vm, 1)
            val2 = get_param_value(vm, 2)
            dest_addr = get_param_address(vm, 3)
            write_mem!(vm, dest_addr, val1 == val2 ? 1 : 0)
            vm.ip += 4
        elseif opcode == 9 # Adjust relative base
             offset = get_param_value(vm, 1)
             vm.relative_base += offset
             vm.ip += 2
        elseif opcode == 99 # Halt
            vm.halted = true
            return
        else
            error("Unknown opcode: $opcode at ip=$(vm.ip)")
        end
    end
end

mutable struct Robot
    position::Tuple{Int, Int}
    direction::Int # 0: Up, 1: Right, 2: Down, 3: Left
end

Robot() = Robot((0, 0), 0) # Constructor with defaults

function turn_and_move!(robot::Robot, turn_direction::Int)
    if turn_direction == 0 # Turn left
        robot.direction = mod(robot.direction - 1, 4)
    else # Turn right (turn_direction == 1)
        robot.direction = mod(robot.direction + 1, 4)
    end

    x, y = robot.position
    if robot.direction == 0 # Up
        robot.position = (x, y - 1)
    elseif robot.direction == 1 # Right
        robot.position = (x + 1, y)
    elseif robot.direction == 2 # Down
        robot.position = (x, y + 1)
    else # robot.direction == 3, Left
        robot.position = (x - 1, y)
    end
end


function main()
    program_str = read("input.txt", String)
    program = parse.(Int, split(strip(program_str), ','))

    grid = Dict{Tuple{Int, Int}, Int}()
    robot = Robot()
    intcode = Intcode(program)

    while !intcode.halted
        current_pos = robot.position
        current_color = get(grid, current_pos, 0) # Default to black (0)

        add_input!(intcode, current_color)
        run!(intcode)
        outputs = intcode.output

        if length(outputs) == 2
            new_color = outputs[1]
            turn_dir = outputs[2]

            grid[current_pos] = new_color
            turn_and_move!(robot, turn_dir)
        elseif !intcode.halted && isempty(intcode.output)
            # Intcode paused without output, likely waiting for more input, should not happen here normally
             break
        elseif !intcode.halted
             # Received partial output? Unexpected for this problem
             error("Intcode produced unexpected output count: $(length(outputs))")
        end
    end

    println(length(grid))
end

main()

