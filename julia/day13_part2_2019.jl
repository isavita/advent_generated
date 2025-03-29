
using Printf

mutable struct IntcodeComputer
    memory::Dict{Int, Int}
    ip::Int
    relative_base::Int
end

function IntcodeComputer(program::Vector{Int})
    memory = Dict{Int, Int}()
    for (i, v) in enumerate(program)
        memory[i-1] = v # 0-based indexing
    end
    return IntcodeComputer(memory, 0, 0)
end

function get_mem(computer::IntcodeComputer, address::Int)
    return get(computer.memory, address, 0)
end

function set_mem!(computer::IntcodeComputer, address::Int, value::Int)
    computer.memory[address] = value
end

function get_parameter(computer::IntcodeComputer, offset::Int, mode::Int)
    param_addr = computer.ip + offset
    if mode == 0 # Position
        addr = get_mem(computer, param_addr)
        return get_mem(computer, addr)
    elseif mode == 1 # Immediate
        return get_mem(computer, param_addr)
    elseif mode == 2 # Relative
        addr = get_mem(computer, param_addr) + computer.relative_base
        return get_mem(computer, addr)
    else
        error("Invalid parameter mode: $mode")
    end
end

function get_write_address(computer::IntcodeComputer, offset::Int, mode::Int)
     param_addr = computer.ip + offset
    if mode == 0 # Position
        return get_mem(computer, param_addr)
    elseif mode == 2 # Relative
        return get_mem(computer, param_addr) + computer.relative_base
    else
         error("Invalid write address mode: $mode")
    end
end

function execute!(computer::IntcodeComputer)
    while true
        instruction = get_mem(computer, computer.ip)
        opcode = instruction % 100
        modes = [(instruction ÷ 10^i) % 10 for i in 2:4] # 1st, 2nd, 3rd param modes

        if opcode == 99 # Halt
            return :halt, nothing
        end

        # Opcode processing
        if opcode == 1 # Add
            p1 = get_parameter(computer, 1, modes[1])
            p2 = get_parameter(computer, 2, modes[2])
            addr = get_write_address(computer, 3, modes[3])
            set_mem!(computer, addr, p1 + p2)
            computer.ip += 4
        elseif opcode == 2 # Multiply
            p1 = get_parameter(computer, 1, modes[1])
            p2 = get_parameter(computer, 2, modes[2])
            addr = get_write_address(computer, 3, modes[3])
            set_mem!(computer, addr, p1 * p2)
            computer.ip += 4
        elseif opcode == 3 # Input
            addr = get_write_address(computer, 1, modes[1])
            # Signal that input is required at addr
            return :input, addr
            # IP will be advanced by caller after input is provided
        elseif opcode == 4 # Output
            output_value = get_parameter(computer, 1, modes[1])
            computer.ip += 2
            return :output, output_value
        elseif opcode == 5 # Jump-if-true
            p1 = get_parameter(computer, 1, modes[1])
            p2 = get_parameter(computer, 2, modes[2])
            if p1 != 0
                computer.ip = p2
            else
                computer.ip += 3
            end
        elseif opcode == 6 # Jump-if-false
            p1 = get_parameter(computer, 1, modes[1])
            p2 = get_parameter(computer, 2, modes[2])
            if p1 == 0
                computer.ip = p2
            else
                computer.ip += 3
            end
        elseif opcode == 7 # Less than
            p1 = get_parameter(computer, 1, modes[1])
            p2 = get_parameter(computer, 2, modes[2])
            addr = get_write_address(computer, 3, modes[3])
            set_mem!(computer, addr, Int(p1 < p2))
            computer.ip += 4
        elseif opcode == 8 # Equals
            p1 = get_parameter(computer, 1, modes[1])
            p2 = get_parameter(computer, 2, modes[2])
            addr = get_write_address(computer, 3, modes[3])
            set_mem!(computer, addr, Int(p1 == p2))
            computer.ip += 4
        elseif opcode == 9 # Adjust relative base
            p1 = get_parameter(computer, 1, modes[1])
            computer.relative_base += p1
            computer.ip += 2
        else
            error("Unknown opcode: $opcode at ip=$(computer.ip)")
        end
    end
end

function parse_input(file_path::String)
    s = read(file_path, String)
    return [parse(Int, n) for n in split(strip(s), ',')]
end

function play_game(program::Vector{Int})
    computer = IntcodeComputer(program)
    computer.memory[0] = 2 # Free play mode

    score = 0
    ball_x = 0
    paddle_x = 0
    output_buffer = Int[]

    while true
        status, value = execute!(computer)

        if status == :output
            push!(output_buffer, value)
            if length(output_buffer) == 3
                x, y, tile_id = output_buffer
                empty!(output_buffer) # Clear buffer

                if x == -1 && y == 0
                    score = tile_id
                else
                    if tile_id == 3 # Paddle
                        paddle_x = x
                    elseif tile_id == 4 # Ball
                        ball_x = x
                    end
                end
            end
        elseif status == :input
            input_addr = value
            joystick_move = sign(ball_x - paddle_x) # Move towards the ball
            set_mem!(computer, input_addr, joystick_move)
            computer.ip += 2 # Advance IP after handling input
        elseif status == :halt
            break # Game finished
        end
    end
    return score
end

function main()
    program = parse_input("input.txt")
    final_score = play_game(program)
    println(final_score)
end

main()
