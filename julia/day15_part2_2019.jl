
using DataStructures

mutable struct Machine
    data::Dict{Int, Int}
    ip::Int
    input_queue::Vector{Int}
    output_queue::Vector{Int}
    relbase::Int
end

function Machine(program::Vector{Int}, input_queue::Vector{Int}, output_queue::Vector{Int})
    data = Dict{Int, Int}(i - 1 => program[i] for i in 1:length(program))
    return Machine(data, 0, input_queue, output_queue, 0)
end

function get_mem(machine::Machine, addr::Int)
    return get(machine.data, addr, 0)
end

function get_val(machine::Machine, i::Int, mo::Int)
    addr = machine.ip + i
    if mo == 0       # Position mode
        return get_mem(machine, get_mem(machine, addr))
    elseif mo == 1   # Immediate mode
        return get_mem(machine, addr)
    elseif mo == 2   # Relative mode
        return get_mem(machine, machine.relbase + get_mem(machine, addr))
    else
        error("Unknown mode: $mo")
    end
end

function set_val(machine::Machine, i::Int, mo::Int, val::Int)
    addr = machine.ip + i
    if mo == 0       # Position mode
        machine.data[get_mem(machine, addr)] = val
    elseif mo == 2   # Relative mode
        machine.data[machine.relbase + get_mem(machine, addr)] = val
    else
        error("Unknown mode for setting: $mo")
    end
end

function step!(machine::Machine)
    instr = get_mem(machine, machine.ip)
    op = instr % 100
    modes = [(instr ÷ 10^(i+1)) % 10 for i in 1:3] # modes[1] is for param 1, etc.

    if op == 1      # add
        val1 = get_val(machine, 1, modes[1])
        val2 = get_val(machine, 2, modes[2])
        set_val(machine, 3, modes[3], val1 + val2)
        machine.ip += 4
    elseif op == 2  # multiply
        val1 = get_val(machine, 1, modes[1])
        val2 = get_val(machine, 2, modes[2])
        set_val(machine, 3, modes[3], val1 * val2)
        machine.ip += 4
    elseif op == 3  # input
        if isempty(machine.input_queue)
            return false # Waiting for input
        end
        input_val = popfirst!(machine.input_queue)
        set_val(machine, 1, modes[1], input_val)
        machine.ip += 2
    elseif op == 4  # output
        output_val = get_val(machine, 1, modes[1])
        push!(machine.output_queue, output_val)
        machine.ip += 2
    elseif op == 5  # jump-if-true
        val1 = get_val(machine, 1, modes[1])
        val2 = get_val(machine, 2, modes[2])
        if val1 != 0
            machine.ip = val2
        else
            machine.ip += 3
        end
    elseif op == 6  # jump-if-false
        val1 = get_val(machine, 1, modes[1])
        val2 = get_val(machine, 2, modes[2])
        if val1 == 0
            machine.ip = val2
        else
            machine.ip += 3
        end
    elseif op == 7  # less than
        val1 = get_val(machine, 1, modes[1])
        val2 = get_val(machine, 2, modes[2])
        set_val(machine, 3, modes[3], val1 < val2 ? 1 : 0)
        machine.ip += 4
    elseif op == 8  # equals
        val1 = get_val(machine, 1, modes[1])
        val2 = get_val(machine, 2, modes[2])
        set_val(machine, 3, modes[3], val1 == val2 ? 1 : 0)
        machine.ip += 4
    elseif op == 9  # relative base offset
        val1 = get_val(machine, 1, modes[1])
        machine.relbase += val1
        machine.ip += 2
    elseif op == 99 # halt
        return false
    else
        error("Unknown opcode: $op at ip $(machine.ip)")
    end
    return true
end

function run!(machine::Machine)
    while step!(machine)
        # continue running
    end
end

manhattan(p::Tuple{Int, Int}, q::Tuple{Int, Int}) = abs(p[1]-q[1]) + abs(p[2]-q[2])

mutable struct Pathfinder
    machine::Machine
    grid::Dict{Tuple{Int, Int}, Char}
    dirmap::Dict{Int, Tuple{Int, Int}}
    p::Tuple{Int, Int}
    oxygen::Union{Nothing, Tuple{Int, Int}}
end

function Pathfinder(program::Vector{Int})
    machine = Machine(program, Int[], Int[])
    grid = Dict((0, 0) => '.')
    dirmap = Dict(1 => (0, 1), 2 => (0, -1), 3 => (-1, 0), 4 => (1, 0)) # N, S, W, E
    p = (0, 0)
    oxygen = nothing
    return Pathfinder(machine, grid, dirmap, p, oxygen)
end

function try_move!(pf::Pathfinder, dir::Int)
    push!(pf.machine.input_queue, dir)
    run!(pf.machine)
    if isempty(pf.machine.output_queue)
        error("Machine halted unexpectedly during move attempt") # Should not happen if exploration is correct
    end
    output = popfirst!(pf.machine.output_queue)
    delta = pf.dirmap[dir]
    next_pos = (pf.p[1] + delta[1], pf.p[2] + delta[2])

    if output == 0
        pf.grid[next_pos] = '#'
        return false
    elseif output == 1
        pf.grid[next_pos] = '.'
    elseif output == 2
        pf.grid[next_pos] = 'O'
        pf.oxygen = next_pos
    end

    pf.p = next_pos
    return true
end

function open_tiles(pf::Pathfinder)
    frontier = Set{Tuple{Int, Int}}()
    for (pos, val) in pf.grid
        if val != '#'
            for delta in values(pf.dirmap)
                next_pos = (pos[1] + delta[1], pos[2] + delta[2])
                if !haskey(pf.grid, next_pos)
                    push!(frontier, pos)
                    break # Only need one unknown neighbor to mark pos as open
                end
            end
        end
    end
    return frontier
end

function shortest_path(pf::Pathfinder, start::Tuple{Int, Int}, finish::Tuple{Int, Int})
    queue = PriorityQueue{Tuple{Tuple{Int, Int}, Vector{Int}}, Int}()
    enqueue!(queue, (start, Int[]), 0)
    visited = Set{Tuple{Int, Int}}()

    while !isempty(queue)
        (pos, path), dist = peek(queue)
        dequeue!(queue)

        if pos == finish
            return path
        end
        if pos in visited
            continue
        end
        push!(visited, pos)

        for (dir, delta) in pf.dirmap
            next_pos = (pos[1] + delta[1], pos[2] + delta[2])
            if haskey(pf.grid, next_pos) && pf.grid[next_pos] != '#' && !(next_pos in visited)
                 enqueue!(queue, (next_pos, vcat(path, [dir])), dist + 1)
            end
        end
    end
    error("No path found from $start to $finish")
end


function explore!(pf::Pathfinder)
     queue = [(0,0)] # Start BFS from origin
     visited = Set([(0,0)])
     paths = Dict((0,0) => Int[]) # Store paths to reconstruct moves

     while !isempty(queue)
         curr_pos = popfirst!(queue)

         # Ensure we are physically at curr_pos
         if pf.p != curr_pos
            path_to_curr = shortest_path(pf, pf.p, curr_pos)
             for move in path_to_curr
                 if !try_move!(pf, move)
                     error("Exploration failed: blocked path $curr_pos from $(pf.p)")
                 end
             end
         end
         @assert pf.p == curr_pos

         # Try all directions from current position
         for dir in 1:4
             delta = pf.dirmap[dir]
             next_pos = (curr_pos[1] + delta[1], curr_pos[2] + delta[2])

             if !haskey(pf.grid, next_pos) # If unknown, try moving
                 moved = try_move!(pf, dir)
                 if moved # Successfully moved to next_pos
                     if !(next_pos in visited)
                          push!(visited, next_pos)
                          push!(queue, next_pos)
                     end
                     # Move back to curr_pos to explore other directions
                     reverse_dir = Dict(1=>2, 2=>1, 3=>4, 4=>3)[dir]
                     if !try_move!(pf, reverse_dir)
                         error("Exploration failed: cannot move back from $next_pos")
                     end
                 else # Hit a wall, grid updated, don't explore from wall
                     push!(visited, next_pos) # Mark wall as visited too
                 end
             elseif pf.grid[next_pos] != '#' && !(next_pos in visited) # Known, not wall, not visited
                 push!(visited, next_pos)
                 push!(queue, next_pos)
             end
         end
     end
 end


function longest_path(pf::Pathfinder, start::Tuple{Int, Int})
    queue = Tuple{Int, Tuple{Int, Int}}[(0, start)] # (distance, position)
    distances = Dict{Tuple{Int, Int}, Int}(start => 0)
    max_dist = 0

    while !isempty(queue)
        dist, pos = popfirst!(queue)
        max_dist = max(max_dist, dist)

        for delta in values(pf.dirmap)
            next_pos = (pos[1] + delta[1], pos[2] + delta[2])
            if haskey(pf.grid, next_pos) && pf.grid[next_pos] != '#' && !haskey(distances, next_pos)
                 distances[next_pos] = dist + 1
                 push!(queue, (dist + 1, next_pos))
            end
        end
    end
    return max_dist
end

function main()
    program_str = open("input.txt") do f
        strip(read(f, String))
    end
    program = parse.(Int, split(program_str, ','))

    pathfinder = Pathfinder(program)
    explore!(pathfinder)

    if isnothing(pathfinder.oxygen)
        error("Oxygen system not found during exploration")
    end

    result = longest_path(pathfinder, pathfinder.oxygen)
    println(result)
end

main()

