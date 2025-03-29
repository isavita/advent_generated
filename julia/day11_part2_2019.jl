
using Printf

mutable struct IntcodeComputer
    memory::Dict{Int, Int}
    ip::Int
    relative_base::Int
    halted::Bool
end

function IntcodeComputer(program::Vector{Int})
    memory = Dict{Int, Int}()
    for (i, val) in enumerate(program)
        memory[i-1] = val
    end
    return IntcodeComputer(memory, 0, 0, false)
end

function mem_read(computer::IntcodeComputer, addr::Int)
    return get(computer.memory, addr, 0)
end

function mem_write(computer::IntcodeComputer, addr::Int, value::Int)
    computer.memory[addr] = value
end

function get_parameter(computer::IntcodeComputer, mode::Int, offset::Int)::Int
    param_addr = computer.ip + offset
    param = mem_read(computer, param_addr)
    if mode == 0 # Position mode
        return mem_read(computer, param)
    elseif mode == 1 # Immediate mode
        return param
    elseif mode == 2 # Relative mode
        return mem_read(computer, computer.relative_base + param)
    else
        error("Unknown parameter mode: $mode")
    end
end

function set_parameter(computer::IntcodeComputer, mode::Int, offset::Int, value::Int)
    param_addr = computer.ip + offset
    param = mem_read(computer, param_addr)
    if mode == 0 # Position mode
        mem_write(computer, param, value)
    elseif mode == 2 # Relative mode
        mem_write(computer, computer.relative_base + param, value)
    else
        error("Unknown parameter mode for writing: $mode")
    end
end

function run!(computer::IntcodeComputer, input_ch::Channel{Int}, output_ch::Channel{Int})
    try
        while !computer.halted
            instruction = mem_read(computer, computer.ip)
            opcode = instruction % 100
            modes = digits(instruction ÷ 100; base=10, pad=3) # modes[1]=mode P1, modes[2]=mode P2, modes[3]=mode P3

            if opcode == 1 # Addition
                p1 = get_parameter(computer, modes[1], 1)
                p2 = get_parameter(computer, modes[2], 2)
                set_parameter(computer, modes[3], 3, p1 + p2)
                computer.ip += 4
            elseif opcode == 2 # Multiplication
                p1 = get_parameter(computer, modes[1], 1)
                p2 = get_parameter(computer, modes[2], 2)
                set_parameter(computer, modes[3], 3, p1 * p2)
                computer.ip += 4
            elseif opcode == 3 # Input
                try
                    input_val = take!(input_ch)
                    set_parameter(computer, modes[1], 1, input_val)
                    computer.ip += 2
                catch e
                    if isa(e, InvalidStateException)
                        computer.halted = true # Assume halt if input channel is closed
                    else
                        rethrow(e)
                    end
                end
            elseif opcode == 4 # Output
                output_val = get_parameter(computer, modes[1], 1)
                try
                    put!(output_ch, output_val)
                    computer.ip += 2
                catch e
                     if isa(e, InvalidStateException)
                        computer.halted = true # Assume halt if output channel is closed
                    else
                        rethrow(e)
                    end
                end
            elseif opcode == 5 # Jump-if-true
                p1 = get_parameter(computer, modes[1], 1)
                p2 = get_parameter(computer, modes[2], 2)
                computer.ip = (p1 != 0) ? p2 : (computer.ip + 3)
            elseif opcode == 6 # Jump-if-false
                p1 = get_parameter(computer, modes[1], 1)
                p2 = get_parameter(computer, modes[2], 2)
                computer.ip = (p1 == 0) ? p2 : (computer.ip + 3)
            elseif opcode == 7 # Less than
                p1 = get_parameter(computer, modes[1], 1)
                p2 = get_parameter(computer, modes[2], 2)
                set_parameter(computer, modes[3], 3, Int(p1 < p2))
                computer.ip += 4
            elseif opcode == 8 # Equals
                p1 = get_parameter(computer, modes[1], 1)
                p2 = get_parameter(computer, modes[2], 2)
                set_parameter(computer, modes[3], 3, Int(p1 == p2))
                computer.ip += 4
            elseif opcode == 9 # Adjust relative base
                p1 = get_parameter(computer, modes[1], 1)
                computer.relative_base += p1
                computer.ip += 2
            elseif opcode == 99 # Halt
                computer.halted = true
                break
            else
                error("Unknown opcode: $opcode at ip $(computer.ip)")
            end
        end
    finally
        close(output_ch)
    end
end

mutable struct Robot
    computer::IntcodeComputer
    direction::Int # 0: Up, 1: Right, 2: Down, 3: Left
    position::Tuple{Int, Int}
    panels::Dict{Tuple{Int, Int}, Int}
    painted_panels::Set{Tuple{Int, Int}}
end

function Robot(program::Vector{Int}, start_panel_color::Int = 0)
    computer = IntcodeComputer(copy(program)) # Use a copy for each robot
    panels = Dict{Tuple{Int, Int}, Int}()
    position = (0, 0)
    panels[position] = start_panel_color
    painted_panels = Set{Tuple{Int, Int}}()
    return Robot(computer, 0, position, panels, painted_panels)
end

function turn_and_move!(robot::Robot, turn_direction::Int)
    if turn_direction == 0 # Turn left
        robot.direction = mod(robot.direction - 1, 4)
    elseif turn_direction == 1 # Turn right
        robot.direction = mod(robot.direction + 1, 4)
    else
        error("Unknown turn direction: $turn_direction")
    end

    x, y = robot.position
    if robot.direction == 0 # Up
        robot.position = (x, y - 1)
    elseif robot.direction == 1 # Right
        robot.position = (x + 1, y)
    elseif robot.direction == 2 # Down
        robot.position = (x, y + 1)
    elseif robot.direction == 3 # Left
        robot.position = (x - 1, y)
    end
end

function run!(robot::Robot)
    input_ch = Channel{Int}(1)
    output_ch = Channel{Int}(1)

    computer_task = @async run!(robot.computer, input_ch, output_ch)

    while true
        current_color = get(robot.panels, robot.position, 0)
        try
            put!(input_ch, current_color)
        catch e
            if isa(e, InvalidStateException) # Channel closed by computer task
                break
            else
                rethrow(e)
            end
        end

        paint_color_maybe = take!(output_ch)
        # Check if computer halted after potentially consuming last input
        if !isopen(output_ch) && !isready(output_ch) 
            break
        end
        paint_color = paint_color_maybe::Int

        turn_direction_maybe = take!(output_ch)
        # Check if computer halted after painting
         if !isopen(output_ch) && !isready(output_ch)
             # Process last paint action before breaking
             robot.panels[robot.position] = paint_color
             push!(robot.painted_panels, robot.position)
             break
        end
        turn_direction = turn_direction_maybe::Int

        robot.panels[robot.position] = paint_color
        push!(robot.painted_panels, robot.position)
        turn_and_move!(robot, turn_direction)
    end
    
    # Ensure computer task finishes, closing input channel might help
    close(input_ch)
    # Optional: wait(computer_task) # Can help catch errors in the computer task
end

function get_painted_panels_count(robot::Robot)::Int
    return length(robot.painted_panels)
end

function render_panels(robot::Robot)
    if isempty(robot.panels)
        println("No panels painted.")
        return
    end

    coords = keys(robot.panels)
    min_x = minimum(p[1] for p in coords; init=0)
    max_x = maximum(p[1] for p in coords; init=0)
    min_y = minimum(p[2] for p in coords; init=0)
    max_y = maximum(p[2] for p in coords; init=0)

    println("\nRegistration Identifier:")
    for y in min_y:max_y
        line = ""
        for x in min_x:max_x
            color = get(robot.panels, (x, y), 0)
            line *= (color == 1 ? "#" : " ")
        end
        println(line)
    end
end

function parse_input(file_path::String)::Vector{Int}
    content = read(file_path, String)
    return [parse(Int, x) for x in split(strip(content), ',')]
end

function main()
    program = parse_input("input.txt")

    # Part One
    robot_part1 = Robot(program, 0)
    run!(robot_part1)
    painted_count_part1 = get_painted_panels_count(robot_part1)
    println("Part One: $painted_count_part1 panels painted at least once.")

    # Part Two
    robot_part2 = Robot(program, 1)
    run!(robot_part2)
    println("Part Two: Registration identifier painted on the hull.")
    render_panels(robot_part2)
end

main()
