
using Printf

mutable struct Machine
    data::Dict{Int, Int}
    ip::Int
    in_stream::Vector{Int}
    in_idx::Int
    out_stream::Vector{Int}
    relbase::Int
end

function Machine(program::Vector{Int}, in_stream::Vector{Int})
    data = Dict{Int, Int}(i - 1 => program[i] for i in 1:length(program))
    return Machine(data, 0, in_stream, 1, Int[], 0)
end

function decode(n::Int)
    op = n % 100
    modes = ( (n ÷ 100) % 10, (n ÷ 1000) % 10, (n ÷ 10000) % 10 )
    return op, modes
end

function get_val(m::Machine, i::Int, mo::Int)
    addr = m.ip + i
    param = get(m.data, addr, 0)
    if mo == 0       # Position Mode
        return get(m.data, param, 0)
    elseif mo == 1   # Immediate Mode
        return param
    elseif mo == 2   # Relative Mode
        return get(m.data, m.relbase + param, 0)
    end
    error("Invalid mode")
end

function set_val(m::Machine, i::Int, mo::Int, val::Int)
    addr = m.ip + i
    param = get(m.data, addr, 0)
    if mo == 0       # Position Mode
        m.data[param] = val
    elseif mo == 2   # Relative Mode
        m.data[m.relbase + param] = val
    else
         error("Invalid mode for setting value")
    end
end

function step!(m::Machine)
    instruction = get(m.data, m.ip, 0)
    op, modes = decode(instruction)

    if op == 1      # add
        val = get_val(m, 1, modes[1]) + get_val(m, 2, modes[2])
        set_val(m, 3, modes[3], val)
        m.ip += 4
    elseif op == 2  # mul
        val = get_val(m, 1, modes[1]) * get_val(m, 2, modes[2])
        set_val(m, 3, modes[3], val)
        m.ip += 4
    elseif op == 3  # input
        if m.in_idx > length(m.in_stream)
             error("Input stream exhausted")
        end
        input_val = m.in_stream[m.in_idx]
        m.in_idx += 1
        set_val(m, 1, modes[1], input_val)
        m.ip += 2
    elseif op == 4  # output
        output_val = get_val(m, 1, modes[1])
        push!(m.out_stream, output_val)
        m.ip += 2
    elseif op == 5  # jt
        if get_val(m, 1, modes[1]) != 0
            m.ip = get_val(m, 2, modes[2])
        else
            m.ip += 3
        end
    elseif op == 6  # jf
        if get_val(m, 1, modes[1]) == 0
            m.ip = get_val(m, 2, modes[2])
        else
            m.ip += 3
        end
    elseif op == 7  # lt
        val = Int(get_val(m, 1, modes[1]) < get_val(m, 2, modes[2]))
        set_val(m, 3, modes[3], val)
        m.ip += 4
    elseif op == 8  # eq
        val = Int(get_val(m, 1, modes[1]) == get_val(m, 2, modes[2]))
        set_val(m, 3, modes[3], val)
        m.ip += 4
    elseif op == 9  # rbo
        m.relbase += get_val(m, 1, modes[1])
        m.ip += 2
    elseif op == 99 # halt
        return false
    else
        error("Unknown opcode: $op at ip $(m.ip)")
    end
    return true
end

function run!(m::Machine)
    while step!(m)
    end
end

function parse_output(output::Vector{Int})
    scaffolding = Set{Tuple{Int, Int}}()
    robot_pos = (-1, -1)
    robot_dir = -1
    x, y = 0, 0
    for o in output
        c = Char(o)
        if c == '\n'
            y += 1
            x = 0
        else
            pos = (x, y)
            if c == '#'
                push!(scaffolding, pos)
            elseif c in ['^', 'v', '<', '>']
                push!(scaffolding, pos)
                robot_pos = pos
                robot_dir = Dict('^' => 0, '>' => 1, 'v' => 2, '<' => 3)[c]
            end
            x += 1
        end
    end
    return scaffolding, robot_pos, robot_dir
end

function sum_align(grid::Set{Tuple{Int, Int}})
    sum_ = 0
    neighbors = [(0, 1), (0, -1), (1, 0), (-1, 0)]
    for (x, y) in grid
        is_intersection = true
        for (dx, dy) in neighbors
            if (x + dx, y + dy) ∉ grid
                is_intersection = false
                break
            end
        end
        if is_intersection
            sum_ += x * y
        end
    end
    return sum_
end

function main()
    program_str = read("input.txt", String)
    program = parse.(Int, split(strip(program_str), ','))

    machine = Machine(program, Int[])
    run!(machine)
    output = machine.out_stream

    scaffolding, _, _ = parse_output(output)
    result = sum_align(scaffolding)
    println(result)
end

main()
