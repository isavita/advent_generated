# Define directions for easy access and manipulation
const DIRECTIONS = [(0, -1), (1, 0), (0, 1), (-1, 0)]  # Up, Right, Down, Left

function read_input(filename)
    grid = Dict()
    lines = readlines(filename)
    center = div(length(lines), 2)
    for (i, line) in enumerate(lines)
        for (j, char) in enumerate(line)
            grid[(j - 1 - center, i - 1 - center)] = char
        end
    end
    return grid
end

function perform_bursts(grid, bursts)
    position = (0, 0)
    direction_index = 0  # Start facing up
    infection_count = 0

    for _ in 1:bursts
        # Turn based on current node state
        if get(grid, position, '.') == '#'
            direction_index = (direction_index + 1) % 4  # Turn right
        else
            direction_index = (direction_index - 1 + 4) % 4  # Turn left
        end

        # Infect or clean the current node
        if get(grid, position, '.') == '#'
            grid[position] = '.'
        else
            grid[position] = '#'
            infection_count += 1
        end

        # Move to the next position
        move = DIRECTIONS[direction_index + 1]
        position = (position[1] + move[1], position[2] + move[2])
    end

    return infection_count
end

function main()
    grid = read_input("input.txt")
    result = perform_bursts(grid, 10000)
    println("Bursts causing infection: $result")
end

main()