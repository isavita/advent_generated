function parse_instructions(filename)
    open(filename, "r") do file
        return split(readline(file), ", ")
    end
end

function navigate(instructions)
    # Directions: 1 = North, 2 = East, 3 = South, 4 = West
    direction = 1
    x, y = 0, 0
    visited = Set([(0, 0)])
    first_revisit_distance = nothing

    for instruction in instructions
        turn = instruction[1]
        blocks = parse(Int, instruction[2:end])

        # Update direction based on turn
        if turn == 'R'
            direction = mod(direction, 4) + 1
        elseif turn == 'L'
            direction = (direction == 1) ? 4 : direction - 1
        end

        # Move in the current direction
        for _ in 1:blocks
            if direction == 1
                y += 1
            elseif direction == 2
                x += 1
            elseif direction == 3
                y -= 1
            elseif direction == 4
                x -= 1
            end

            # Check for revisits
            if (x, y) in visited && first_revisit_distance === nothing
                first_revisit_distance = abs(x) + abs(y)
            end
            push!(visited, (x, y))
        end
    end

    final_distance = abs(x) + abs(y)
    return final_distance, first_revisit_distance
end

function main()
    instructions = parse_instructions("input.txt")
    final_distance, first_revisit_distance = navigate(instructions)
    println("Distance to final destination: $final_distance")
    println("Distance to first revisited location: $first_revisit_distance")
end

main()