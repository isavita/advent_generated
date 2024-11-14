
using DataStructures

mutable struct Halves
    is_chip::Bool
    material::String
end

mutable struct State
    floors::Vector{Vector{Halves}}
    elevator_level::Int
    steps::Int
end

function Base.hash(h::Halves, h0::UInt)
    return hash(h.is_chip, hash(h.material, h0))
end

function Base.isequal(h1::Halves, h2::Halves)
    return h1.is_chip == h2.is_chip && h1.material == h2.material
end

function new_initial_state(input::String)
    lines = split(input, "\n")
    floors = [Halves[] for _ in 1:4]
    
    for (line_index, line) in enumerate(lines)
        parts = split(line, " ")
        parts = [replace(p, r"[,.]" => "") for p in parts]
        
        for i in 1:length(parts)
            if parts[i] == "generator"
                push!(floors[line_index], Halves(false, parts[i-1]))
            elseif parts[i] == "microchip"
                material = split(parts[i-1], "-")[1]
                push!(floors[line_index], Halves(true, material))
            end
        end
    end
    
    return State(floors, 1, 0)
end

function is_valid(s::State)
    for floor in s.floors
        gens = Set{String}()
        for half in floor
            if !half.is_chip
                push!(gens, half.material)
            end
        end
        
        if !isempty(gens)
            for half in floor
                if half.is_chip && half.material ∉ gens
                    return false
                end
            end
        end
    end
    return true
end

function is_done(s::State)
    return sum(length(floor) for floor in s.floors[1:3]) == 0
end

function hash_key(s::State)
    gen_indices = Dict{String, Int}()
    chip_indices = Dict{String, Int}()
    
    for (floor_idx, floor) in enumerate(s.floors)
        for half in floor
            if half.is_chip
                chip_indices[half.material] = floor_idx
            else
                gen_indices[half.material] = floor_idx
            end
        end
    end
    
    gen_chip_pairs = [(get(gen_indices, mat, 0), get(chip_indices, mat, 0)) 
                      for mat in union(keys(gen_indices), keys(chip_indices))]
    
    sort!(gen_chip_pairs)
    return (s.elevator_level, gen_chip_pairs)
end

function get_movable_perm_indices(s::State)
    current_level = s.floors[s.elevator_level]
    perms_to_move = Vector{Vector{Int}}()
    
    for i in 1:length(current_level)
        push!(perms_to_move, [i])
        for j in (i+1):length(current_level)
            push!(perms_to_move, [i, j])
        end
    end
    
    return perms_to_move
end

function get_next_states(s::State)
    future_states = State[]
    movable_perm_indices = get_movable_perm_indices(s)
    
    ele_diffs = s.elevator_level < 4 ? [1] : []
    append!(ele_diffs, s.elevator_level > 1 ? [-1] : [])
    
    for ele_diff in ele_diffs
        for perm_indices in movable_perm_indices
            cl = deepcopy(s)
            cl.elevator_level += ele_diff
            cl.steps += 1
            old_level = s.elevator_level
            new_level = cl.elevator_level
            
            for index in perm_indices
                push!(cl.floors[new_level], cl.floors[old_level][index])
            end
            
            deleteat!(cl.floors[old_level], perm_indices)
            
            if is_valid(cl)
                push!(future_states, cl)
            end
        end
    end
    
    return future_states
end

function rtg_hell_day(input::String)
    current_state = new_initial_state(input)
    
    queue = Queue{State}()
    enqueue!(queue, current_state)
    
    prev_states = Set{Any}()
    
    while !isempty(queue)
        front = dequeue!(queue)
        
        if is_done(front)
            return front.steps
        end
        
        key = hash_key(front)
        if key ∈ prev_states
            continue
        end
        push!(prev_states, key)
        
        next_states = get_next_states(front)
        for state in next_states
            enqueue!(queue, state)
        end
    end
    
    return -1
end

function main()
    input = read("input.txt", String)
    ans = rtg_hell_day(input)
    println(ans)
end

main()
