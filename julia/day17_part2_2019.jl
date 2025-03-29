
using DataStructures

mutable struct IntcodeComputer
    memory::Dict{Int, Int}
    pointer::Int
    relative_base::Int
    inputs::Vector{Int}
    outputs::Vector{Int}
    halted::Bool
end

function IntcodeComputer(program::Vector{Int})
    memory = Dict{Int, Int}()
    for (i, val) in enumerate(program)
        memory[i-1] = val
    end
    return IntcodeComputer(memory, 0, 0, Int[], Int[], false)
end

function get_mem(computer::IntcodeComputer, address::Int)
    return get(computer.memory, address, 0)
end

function get_param(computer::IntcodeComputer, mode::Int, param_val::Int)
    if mode == 0       # Position mode
        return get_mem(computer, param_val)
    elseif mode == 1   # Immediate mode
        return param_val
    elseif mode == 2   # Relative mode
        return get_mem(computer, computer.relative_base + param_val)
    else
        error("Unknown parameter mode: $mode")
    end
end

function set_param(computer::IntcodeComputer, mode::Int, param_val::Int, value::Int)
    if mode == 0       # Position mode
        computer.memory[param_val] = value
    elseif mode == 2   # Relative mode
        computer.memory[computer.relative_base + param_val] = value
    else
        error("Unknown parameter mode for writing: $mode")
    end
end

function run!(computer::IntcodeComputer)
    while true
        instruction = get_mem(computer, computer.pointer)
        opcode = instruction % 100
        modes = [(instruction ÷ 100) % 10, (instruction ÷ 1000) % 10, (instruction ÷ 10000) % 10]

        if opcode == 1 # Addition
            p1 = get_mem(computer, computer.pointer + 1)
            p2 = get_mem(computer, computer.pointer + 2)
            p3 = get_mem(computer, computer.pointer + 3)
            v1 = get_param(computer, modes[1], p1)
            v2 = get_param(computer, modes[2], p2)
            set_param(computer, modes[3], p3, v1 + v2)
            computer.pointer += 4
        elseif opcode == 2 # Multiplication
            p1 = get_mem(computer, computer.pointer + 1)
            p2 = get_mem(computer, computer.pointer + 2)
            p3 = get_mem(computer, computer.pointer + 3)
            v1 = get_param(computer, modes[1], p1)
            v2 = get_param(computer, modes[2], p2)
            set_param(computer, modes[3], p3, v1 * v2)
            computer.pointer += 4
        elseif opcode == 3 # Input
            if isempty(computer.inputs)
                return :needs_input # Indicate waiting for input
            end
            p1 = get_mem(computer, computer.pointer + 1)
            input_value = popfirst!(computer.inputs)
            set_param(computer, modes[1], p1, input_value)
            computer.pointer += 2
        elseif opcode == 4 # Output
            p1 = get_mem(computer, computer.pointer + 1)
            output_value = get_param(computer, modes[1], p1)
            push!(computer.outputs, output_value)
            computer.pointer += 2
            # return :output_produced # Can return here if needed externally
        elseif opcode == 5 # Jump-if-true
            p1 = get_mem(computer, computer.pointer + 1)
            p2 = get_mem(computer, computer.pointer + 2)
            v1 = get_param(computer, modes[1], p1)
            v2 = get_param(computer, modes[2], p2)
            if v1 != 0
                computer.pointer = v2
            else
                computer.pointer += 3
            end
        elseif opcode == 6 # Jump-if-false
            p1 = get_mem(computer, computer.pointer + 1)
            p2 = get_mem(computer, computer.pointer + 2)
            v1 = get_param(computer, modes[1], p1)
            v2 = get_param(computer, modes[2], p2)
            if v1 == 0
                computer.pointer = v2
            else
                computer.pointer += 3
            end
        elseif opcode == 7 # Less than
            p1 = get_mem(computer, computer.pointer + 1)
            p2 = get_mem(computer, computer.pointer + 2)
            p3 = get_mem(computer, computer.pointer + 3)
            v1 = get_param(computer, modes[1], p1)
            v2 = get_param(computer, modes[2], p2)
            set_param(computer, modes[3], p3, Int(v1 < v2))
            computer.pointer += 4
        elseif opcode == 8 # Equals
            p1 = get_mem(computer, computer.pointer + 1)
            p2 = get_mem(computer, computer.pointer + 2)
            p3 = get_mem(computer, computer.pointer + 3)
            v1 = get_param(computer, modes[1], p1)
            v2 = get_param(computer, modes[2], p2)
            set_param(computer, modes[3], p3, Int(v1 == v2))
            computer.pointer += 4
        elseif opcode == 9 # Adjust relative base
            p1 = get_mem(computer, computer.pointer + 1)
            v1 = get_param(computer, modes[1], p1)
            computer.relative_base += v1
            computer.pointer += 2
        elseif opcode == 99 # Halt
            computer.halted = true
            return :halted
        else
            error("Unknown opcode: $opcode at pointer $(computer.pointer)")
        end
    end
end

function read_input(filename::String)
    content = read(filename, String)
    return [parse(Int, x) for x in split(strip(content), ',')]
end

function parse_map(output::Vector{Int})
    grid = Vector{Vector{Char}}()
    line = Char[]
    for c_val in output
        if c_val == 10
            if !isempty(line)
                push!(grid, line)
                line = Char[]
            end
        else
            push!(line, Char(c_val))
        end
    end
    if !isempty(line)
        push!(grid, line)
    end
    return grid
end

function find_intersections(grid::Vector{Vector{Char}})
    intersections = Tuple{Int, Int}[]
    height = length(grid)
    width = length(grid[1])
    for y in 2:height-1
        for x in 2:width-1
            if grid[y][x] != '#'
                continue
            end
            if grid[y-1][x] == '#' && grid[y+1][x] == '#' &&
               grid[y][x-1] == '#' && grid[y][x+1] == '#'
                push!(intersections, (x - 1, y - 1)) # 0-based coords for calculation
            end
        end
    end
    return intersections
end

function find_robot_position(grid::Vector{Vector{Char}})
    for (y, row) in enumerate(grid)
        for (x, cell) in enumerate(row)
            if cell in ['^', 'v', '<', '>', 'X']
                return (x, y, cell) # 1-based grid coords
            end
        end
    end
    return nothing
end

const turn_left_map = Dict('^' => '<', '<' => 'v', 'v' => '>', '>' => '^')
const turn_right_map = Dict('^' => '>', '>' => 'v', 'v' => '<', '<' => '^')

turn_left(direction::Char) = turn_left_map[direction]
turn_right(direction::Char) = turn_right_map[direction]

function move_forward(x::Int, y::Int, direction::Char)
    if direction == '^'
        return (x, y - 1)
    elseif direction == 'v'
        return (x, y + 1)
    elseif direction == '<'
        return (x - 1, y)
    elseif direction == '>'
        return (x + 1, y)
    else
        error("Unknown direction: $direction")
    end
end

function get_movement_path(grid::Vector{Vector{Char}}, start_x::Int, start_y::Int, start_dir::Char)
    x, y, direction = start_x, start_y, start_dir
    path = String[]
    steps = 0
    height = length(grid)
    width = length(grid[1])

    while true
        next_x, next_y = move_forward(x, y, direction)
        if 1 <= next_y <= height && 1 <= next_x <= width && grid[next_y][next_x] == '#'
            x, y = next_x, next_y
            steps += 1
        else
            if steps > 0
                push!(path, string(steps))
                steps = 0
            end

            left_dir = turn_left(direction)
            next_lx, next_ly = move_forward(x, y, left_dir)
            if 1 <= next_ly <= height && 1 <= next_lx <= width && grid[next_ly][next_lx] == '#'
                push!(path, "L")
                direction = left_dir
                continue
            end

            right_dir = turn_right(direction)
            next_rx, next_ry = move_forward(x, y, right_dir)
             if 1 <= next_ry <= height && 1 <= next_rx <= width && grid[next_ry][next_rx] == '#'
                push!(path, "R")
                direction = right_dir
                continue
            end
            break # No forward or turn possible
        end
    end
    return path
end


function replace_sequence(seq::Vector{String}, pattern::Vector{String}, replacement::String)
    res = String[]
    i = 1
    n = length(seq)
    p_len = length(pattern)
    while i <= n
        if i + p_len - 1 <= n && seq[i:i+p_len-1] == pattern
            push!(res, replacement)
            i += p_len
        else
            push!(res, seq[i])
            i += 1
        end
    end
    return res
end


function compress_movement(path_tokens::Vector{String})
    max_func_len_chars = 20
    max_func_len_tokens = 10 # Heuristic, adjust if needed

    n_tokens = length(path_tokens)

    for a_len in 1:min(max_func_len_tokens, n_tokens)
        a_pattern = path_tokens[1:a_len]
        a_str = join(a_pattern, ",")
        if length(a_str) > max_func_len_chars continue end

        for b_start_idx in 1:n_tokens
             # Find first token not part of an 'A' sequence starting at or after a_len + 1
            temp_idx = 1
            b_actual_start = -1
            while temp_idx <= n_tokens
                if temp_idx + a_len - 1 <= n_tokens && path_tokens[temp_idx:temp_idx+a_len-1] == a_pattern
                     temp_idx += a_len
                 else
                     if b_actual_start == -1 # First non-A token is potential B start
                         b_actual_start = temp_idx
                     end
                     temp_idx += 1
                 end
             end
             if b_actual_start == -1 continue end # Only A's found

            for b_len in 1:min(max_func_len_tokens, n_tokens - b_actual_start + 1)
                b_pattern = path_tokens[b_actual_start : b_actual_start + b_len - 1]
                if b_pattern == a_pattern continue end # Avoid B being same as A
                b_str = join(b_pattern, ",")
                if length(b_str) > max_func_len_chars continue end

                 for c_start_idx in 1:n_tokens
                     # Find first token not part of A or B
                     temp_idx = 1
                     c_actual_start = -1
                     while temp_idx <= n_tokens
                         is_a = temp_idx + a_len - 1 <= n_tokens && path_tokens[temp_idx:temp_idx+a_len-1] == a_pattern
                         is_b = temp_idx + b_len - 1 <= n_tokens && path_tokens[temp_idx:temp_idx+b_len-1] == b_pattern

                         if is_a
                             temp_idx += a_len
                         elseif is_b
                              temp_idx += b_len
                         else
                              if c_actual_start == -1 # First non-A/B token is potential C start
                                  c_actual_start = temp_idx
                              end
                              temp_idx += 1
                         end
                      end
                      if c_actual_start == -1 continue end # Only A's and B's found

                     for c_len in 1:min(max_func_len_tokens, n_tokens - c_actual_start + 1)
                         c_pattern = path_tokens[c_actual_start : c_actual_start + c_len - 1]
                         if c_pattern == a_pattern || c_pattern == b_pattern continue end # Avoid C being same as A or B
                         c_str = join(c_pattern, ",")
                         if length(c_str) > max_func_len_chars continue end

                         # Try to build main routine with A, B, C
                         main_routine_tokens = String[]
                         current_idx = 1
                         possible = true
                         while current_idx <= n_tokens
                            if current_idx + a_len - 1 <= n_tokens && path_tokens[current_idx:current_idx+a_len-1] == a_pattern
                                 push!(main_routine_tokens, "A")
                                 current_idx += a_len
                             elseif current_idx + b_len - 1 <= n_tokens && path_tokens[current_idx:current_idx+b_len-1] == b_pattern
                                 push!(main_routine_tokens, "B")
                                 current_idx += b_len
                              elseif current_idx + c_len - 1 <= n_tokens && path_tokens[current_idx:current_idx+c_len-1] == c_pattern
                                 push!(main_routine_tokens, "C")
                                 current_idx += c_len
                             else
                                 possible = false
                                 break
                             end
                         end

                         if possible
                             main_routine_str = join(main_routine_tokens, ",")
                             if length(main_routine_str) <= max_func_len_chars
                                 return main_routine_str, a_str, b_str, c_str
                             end
                         end
                     end
                 end
            end
        end
    end

    error("Could not compress the path into functions A, B, C.")
end


function main()
    program = read_input("input.txt")

    # Part One (implicit, needed for map)
    computer = IntcodeComputer(copy(program))
    run!(computer)
    output = computer.outputs
    grid = parse_map(output)

    # Uncomment to print map
    # for row in grid
    #    println(join(row))
    # end

    intersections = find_intersections(grid)
    alignment_sum = sum(x * y for (x, y) in intersections)
    # println("Part One: Sum of alignment parameters = $alignment_sum") # Not requested in final output

    # Part Two
    program_part2 = copy(program)
    program_part2[1] = 2 # Modify program for part 2 (Julia uses 1-based indexing)

    computer_part2 = IntcodeComputer(program_part2)

    robot_info = find_robot_position(grid)
    if isnothing(robot_info)
        error("Robot not found on the scaffold.")
    end
    start_x, start_y, start_dir = robot_info

    movement_path_tokens = get_movement_path(grid, start_x, start_y, start_dir)

    main_routine, function_A, function_B, function_C = compress_movement(movement_path_tokens)

    input_lines = [
        main_routine,
        function_A,
        function_B,
        function_C,
        "n" # Video feed 'no'
    ]

    movement_inputs = Int[]
    for line in input_lines
        for char in line
            push!(movement_inputs, Int(char))
        end
        push!(movement_inputs, 10) # Newline ASCII
    end

    computer_part2.inputs = movement_inputs

    run!(computer_part2) # Run until completion or needs_input (should complete)

    dust_collected = 0
    if !isempty(computer_part2.outputs)
         dust_collected = computer_part2.outputs[end] # The last output is the dust
    end

    println(dust_collected)

end

main()

