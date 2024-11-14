
function parse_input(filename)
    lines = readlines(filename)
    elves = Set{Tuple{Int,Int}}()
    for (y, line) in enumerate(lines)
        for (x, char) in enumerate(line)
            if char == '#'
                push!(elves, (x, y))
            end
        end
    end
    return elves
end

function get_adjacent_positions(pos)
    x, y = pos
    return Set([
        (x-1, y-1), (x, y-1), (x+1, y-1),
        (x-1, y),             (x+1, y),
        (x-1, y+1), (x, y+1), (x+1, y+1)
    ])
end

function propose_move(elves, elf, directions)
    # Check if no adjacent elves
    adjacent = get_adjacent_positions(elf)
    if isempty(intersect(adjacent, elves))
        return elf
    end

    for (dx, dy, check_positions) in directions
        check_set = Set((elf[1] + x, elf[2] + y) for (x, y) in check_positions)
        if isempty(intersect(check_set, elves))
            return (elf[1] + dx, elf[2] + dy)
        end
    end
    return elf
end

function simulate_round!(elves, directions)
    # First half: propose moves
    proposed_moves = Dict{Tuple{Int,Int}, Vector{Tuple{Int,Int}}}()
    for elf in elves
        proposed_move = propose_move(elves, elf, directions)
        push!(get!(proposed_moves, proposed_move, []), elf)
    end

    # Second half: move elves
    new_elves = Set{Tuple{Int,Int}}()
    moved = false
    for (proposed, original_elves) in proposed_moves
        if length(original_elves) == 1
            push!(new_elves, proposed)
            moved |= proposed != first(original_elves)
        else
            union!(new_elves, original_elves)
        end
    end

    return new_elves, moved
end

function solve_part1(filename)
    elves = parse_input(filename)
    directions = [
        (0, -1, [(0, -1), (-1, -1), (1, -1)]),  # North
        (0, 1, [(0, 1), (-1, 1), (1, 1)]),      # South
        (-1, 0, [(-1, 0), (-1, -1), (-1, 1)]),  # West
        (1, 0, [(1, 0), (1, -1), (1, 1)])       # East
    ]

    for _ in 1:10
        elves, _ = simulate_round!(elves, directions)
        # Rotate directions
        directions = [directions[2:end]; directions[1:1]]
    end

    # Calculate empty ground tiles
    min_x = minimum(x for (x, _) in elves)
    max_x = maximum(x for (x, _) in elves)
    min_y = minimum(y for (_, y) in elves)
    max_y = maximum(y for (_, y) in elves)

    return (max_x - min_x + 1) * (max_y - min_y + 1) - length(elves)
end

function solve_part2(filename)
    elves = parse_input(filename)
    directions = [
        (0, -1, [(0, -1), (-1, -1), (1, -1)]),  # North
        (0, 1, [(0, 1), (-1, 1), (1, 1)]),      # South
        (-1, 0, [(-1, 0), (-1, -1), (-1, 1)]),  # West
        (1, 0, [(1, 0), (1, -1), (1, 1)])       # East
    ]

    round = 0
    while true
        round += 1
        elves, moved = simulate_round!(elves, directions)
        
        # Rotate directions
        directions = [directions[2:end]; directions[1:1]]

        # Stop when no elves move
        !moved && break
    end

    return round
end

# Main execution
function main()
    input_file = "input.txt"
    println("Part 1: ", solve_part1(input_file))
    println("Part 2: ", solve_part2(input_file))
end

main()
