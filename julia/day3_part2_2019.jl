using DataStructures

function get_wire_paths(input)
    paths = []
    for line in input
        path = []
        current = (0, 0)
        steps = 0
        for instruction in split(line, ",")
            direction = instruction[1]
            distance = parse(Int, instruction[2:end])
            for _ in 1:distance
                if direction == 'U'
                    current = (current[1], current[2] + 1)
                elseif direction == 'D'
                    current = (current[1], current[2] - 1)
                elseif direction == 'L'
                    current = (current[1] - 1, current[2])
                elseif direction == 'R'
                    current = (current[1] + 1, current[2])
                end
                steps += 1
                push!(path, (current, steps))
            end
        end
        push!(paths, path)
    end
    return paths
end

function find_intersections(paths)
    wire1_path = paths[1]
    wire2_path = paths[2]
    
    intersections = Set()
    
    wire1_steps = Dict()
    for (pos, steps) in wire1_path
        wire1_steps[pos] = steps
    end
    
    for (pos, steps) in wire2_path
        if pos in keys(wire1_steps)
            push!(intersections, pos)
        end
    end
    
    return intersections, wire1_steps, wire2_path
end

function main()
    input = readlines("input.txt")
    paths = get_wire_paths(input)
    intersections, wire1_steps, wire2_path = find_intersections(paths)
    
    min_distance = minimum([abs(x) + abs(y) for (x, y) in intersections])
    println("Part 1: ", min_distance)
    
    min_steps = minimum([wire1_steps[intersection] + steps for (intersection, steps) in wire2_path if intersection in intersections])
    println("Part 2: ", min_steps)
end

main()