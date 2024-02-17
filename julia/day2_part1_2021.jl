
open("input.txt") do file
    horizontalPosition = 0
    depth = 0

    for line in eachline(file)
        command = split(line, " ")
        direction = command[1]
        units = parse(Int, command[2])

        if direction == "forward"
            horizontalPosition += units
        elseif direction == "down"
            depth += units
        elseif direction == "up"
            depth -= units
        end
    end

    product = horizontalPosition * depth
    println(product)
end
