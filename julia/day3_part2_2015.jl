function count_houses_with_presents(input)
    # Initialize positions and a set to track visited houses
    santa_pos = robo_pos = (0, 0)
    visited_houses = Set([(0, 0)])
    
    for (i, direction) in enumerate(input)
        # Determine whose turn it is
        if i % 2 == 1
            santa_pos = move(santa_pos, direction)
            push!(visited_houses, santa_pos)
        else
            robo_pos = move(robo_pos, direction)
            push!(visited_houses, robo_pos)
        end
    end
    
    return length(visited_houses)
end

function move(pos, direction)
    x, y = pos
    if direction == '^'
        return (x, y + 1)
    elseif direction == 'v'
        return (x, y - 1)
    elseif direction == '>'
        return (x + 1, y)
    elseif direction == '<'
        return (x - 1, y)
    end
    return pos
end

# Read input from file
input = read(open("input.txt", "r"), String)

# Clean input (if necessary) and process
clean_input = strip(input)
println(count_houses_with_presents(clean_input))

