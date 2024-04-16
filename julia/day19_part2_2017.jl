function read_routing_diagram(filename)
    return readlines(filename)
end

function find_start_position(diagram)
    # This function should return a valid index or handle the case when '|' is not found
    return findfirst(==( '|'), diagram[1])[1]
end

function navigate_diagram(diagram)
    rows = length(diagram)
    cols = length(diagram[1])
    x, y = 1, find_start_position(diagram)
    dx, dy = 1, 0  # Initial direction is down
    letters = []
    steps = 0

    while x > 0 && x <= rows && y > 0 && y <= cols
        current_char = diagram[x][y]
        if current_char in "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
            push!(letters, current_char)
        elseif current_char == '+'
            # Change direction
            if dx == 0  # Moving horizontally, so need to move vertically
                dx, dy = dy, 0
                # Check new direction is valid
                if x + dx < 1 || x + dx > rows || diagram[x + dx][y] == ' '
                    dx, dy = -dx, dy
                end
            else  # Moving vertically, so need to move horizontally
                dx, dy = 0, dx
                if y + dy < 1 || y + dy > cols || diagram[x][y + dy] == ' '
                    dx, dy = dx, -dy
                end
            end
        elseif current_char == ' '
            break
        end
        x += dx
        y += dy
        steps += 1
    end

    return (join(letters), steps)
end

function main()
    diagram = read_routing_diagram("input.txt")
    result = navigate_diagram(diagram)
    println("Letters: ", result[1])
    println("Steps: ", result[2])
end

main()