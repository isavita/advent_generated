
function parse_input(filename)
    elves = Set{Tuple{Int,Int}}()
    for (y, line) in enumerate(readlines(filename))
        for (x, char) in enumerate(line)
            if char == '#'
                push!(elves, (x-1, y-1))
            end
        end
    end
    return elves
end

function check_direction(elves, pos, direction)
    x, y = pos
    if direction == :N
        return !((x-1, y-1) in elves || (x, y-1) in elves || (x+1, y-1) in elves)
    elseif direction == :S
        return !((x-1, y+1) in elves || (x, y+1) in elves || (x+1, y+1) in elves)
    elseif direction == :W
        return !((x-1, y-1) in elves || (x-1, y) in elves || (x-1, y+1) in elves)
    elseif direction == :E
        return !((x+1, y-1) in elves || (x+1, y) in elves || (x+1, y+1) in elves)
    end
end

function check_adjacent(elves, pos)
    x, y = pos
    for dx in -1:1, dy in -1:1
        (dx == 0 && dy == 0) && continue
        if (x+dx, y+dy) in elves
            return false
        end
    end
    return true
end

function simulate_round!(elves, directions)
    proposed_moves = Dict{Tuple{Int,Int}, Vector{Tuple{Int,Int}}}()
    
    for elf in elves
        if check_adjacent(elves, elf)
            continue
        end
        
        for direction in directions
            if check_direction(elves, elf, direction)
                proposed_pos = if direction == :N
                    (elf[1], elf[2]-1)
                elseif direction == :S
                    (elf[1], elf[2]+1)
                elseif direction == :W
                    (elf[1]-1, elf[2])
                else  # :E
                    (elf[1]+1, elf[2])
                end
                
                push!(get!(proposed_moves, proposed_pos, []), elf)
                break
            end
        end
    end
    
    moved = false
    for (proposed_pos, moving_elves) in proposed_moves
        if length(moving_elves) == 1
            delete!(elves, moving_elves[1])
            push!(elves, proposed_pos)
            moved = true
        end
    end
    
    # Rotate directions
    push!(directions, popfirst!(directions))
    
    return moved
end

function count_empty_ground(elves)
    min_x = minimum(x for (x, _) in elves)
    max_x = maximum(x for (x, _) in elves)
    min_y = minimum(y for (_, y) in elves)
    max_y = maximum(y for (_, y) in elves)
    
    return (max_x - min_x + 1) * (max_y - min_y + 1) - length(elves)
end

function solve_part1(filename)
    elves = parse_input(filename)
    directions = [:N, :S, :W, :E]
    
    for _ in 1:10
        simulate_round!(elves, directions)
    end
    
    return count_empty_ground(elves)
end

# Read input from file and print result
println(solve_part1("input.txt"))
