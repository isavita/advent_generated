function main()
    # Read input from file
    instructions = read_input("input.txt")

    # Define initial conditions
    x, y = 0, 0
    # Facing North initially (0: North, 1: East, 2: South, 3: West)
    direction = 0

    # Process each instruction
    for instruction in instructions
        turn, blocks = parse_instruction(instruction)
        
        # Update direction based on turn
        if turn == 'L'
            direction = (direction + 3) % 4  # Turn left
        else
            direction = (direction + 1) % 4  # Turn right
        end

        # Move in the current direction
        if direction == 0  # North
            y += blocks
        elseif direction == 1  # East
            x += blocks
        elseif direction == 2  # South
            y -= blocks
        elseif direction == 3  # West
            x -= blocks
        end
    end

    # Calculate Manhattan distance
    distance = abs(x) + abs(y)
    println("The distance to the Easter Bunny HQ is $distance blocks.")
end

function read_input(filename)
    open(filename, "r") do file
        return split(readline(file), ", ")
    end
end

function parse_instruction(instruction)
    turn = instruction[1]
    blocks = parse(Int, instruction[2:end])
    return turn, blocks
end

main()