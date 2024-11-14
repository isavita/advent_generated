
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

function Base.hash(s::State)
    gen_indices = Dict{String, Int}()
    chip_indices = Dict{String, Int}()
    
    for (fl_idx, floor) in enumerate(s.floors)
        for half in floor
            if half.is_chip
                chip_indices[half.material] = fl_idx
            else
                gen_indices[half.material] = fl_idx
            end
        end
    end
    
    gen_chip_pairs = [(get(gen_indices, mat, 0), get(chip_indices, mat, 0)) 
                      for mat in union(keys(gen_indices), keys(chip_indices))]
    
    return hash((s.elevator_level, sort(gen_chip_pairs)))
end

function Base.isequal(a::State, b::State)
    hash(a) == hash(b)
end

function is_valid(s::State)
    for floor in s.floors
        gen_seen = Set{String}()
        
        for half in floor
            !half.is_chip && push!(gen_seen, half.material)
        end
        
        isempty(gen_seen) && continue
        
        for half in floor
            if half.is_chip && half.material ∉ gen_seen
                return false
            end
        end
    end
    
    return true
end

function is_done(s::State)
    sum(length(floor) for floor in s.floors[1:3]) == 0
end

function get_movable_perm_indices(s::State)
    current_level = s.floors[s.elevator_level + 1]
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
    future_states = Vector{State}()
    movable_perm_indices = get_movable_perm_indices(s)
    
    ele_diffs = Int[]
    s.elevator_level < 3 && push!(ele_diffs, 1)
    s.elevator_level > 0 && push!(ele_diffs, -1)
    
    for ele_diff in ele_diffs
        for perm_indices in movable_perm_indices
            cl = deepcopy(s)
            cl.elevator_level += ele_diff
            cl.steps += 1
            old_level = s.elevator_level + 1
            new_level = cl.elevator_level + 1
            
            for index in perm_indices
                push!(cl.floors[new_level], cl.floors[old_level][index])
            end
            
            for index in reverse(perm_indices)
                deleteat!(cl.floors[old_level], index)
            end
            
            is_valid(cl) && push!(future_states, cl)
        end
    end
    
    return future_states
end

function rtg_hell_day(input::String)
    current_state = new_initial_state(input)
    push!(current_state.floors[1], 
        Halves(false, "elerium"),
        Halves(true, "elerium"),
        Halves(false, "dilithium"),
        Halves(true, "dilithium")
    )
    
    queue = [current_state]
    prev_states = Set{State}()
    
    while !isempty(queue)
        front = popfirst!(queue)
        
        is_done(front) && return front.steps
        
        front ∈ prev_states && continue
        push!(prev_states, front)
        
        append!(queue, get_next_states(front))
    end
    
    return -1
end

function new_initial_state(input::String)
    s = State([[] for _ in 1:4], 0, 0)
    
    for (line_index, line) in enumerate(split(input, "\n"))
        parts = split(line, " ")
        parts = [strip(p, [',', '.']) for p in parts]
        
        for i in 1:length(parts)
            if parts[i] == "generator"
                material = parts[i-1]
                push!(s.floors[line_index], Halves(false, material))
            elseif parts[i] == "microchip"
                material = split(parts[i-1], "-")[1]
                push!(s.floors[line_index], Halves(true, material))
            end
        end
    end
    
    return s
end

function main()
    input = read("input.txt", String)
    ans = rtg_hell_day(input)
    println(ans)
end

main()
