
function read_input(filename)
    cubes = Set{Tuple{Int,Int,Int}}()
    open(filename) do file
        for line in eachline(file)
            x, y, z = parse.(Int, split(line, ','))
            push!(cubes, (x, y, z))
        end
    end
    return cubes
end

function surface_area(cubes)
    sides = 0
    for cube in cubes
        for neighbor in adjacent_cubes(cube)
            if neighbor ∉ cubes
                sides += 1
            end
        end
    end
    return sides
end

function adjacent_cubes(cube)
    x, y, z = cube
    return [
        (x+1, y, z), (x-1, y, z),
        (x, y+1, z), (x, y-1, z),
        (x, y, z+1), (x, y, z-1)
    ]
end

function exterior_surface_area(cubes)
    # Find bounding box
    min_x = minimum(x for (x,_,_) in cubes) - 1
    max_x = maximum(x for (x,_,_) in cubes) + 1
    min_y = minimum(y for (_,y,_) in cubes) - 1
    max_y = maximum(y for (_,y,_) in cubes) + 1
    min_z = minimum(z for (_,_,z) in cubes) - 1
    max_z = maximum(z for (_,_,z) in cubes) + 1

    # Flood fill from outside
    visited = Set{Tuple{Int,Int,Int}}()
    queue = [(min_x, min_y, min_z)]
    
    while !isempty(queue)
        current = popfirst!(queue)
        
        if current ∈ visited ||
           current[1] < min_x || current[1] > max_x ||
           current[2] < min_y || current[2] > max_y ||
           current[3] < min_z || current[3] > max_z
            continue
        end
        
        push!(visited, current)
        
        for neighbor in adjacent_cubes(current)
            if neighbor ∉ visited && neighbor ∉ cubes
                push!(queue, neighbor)
            end
        end
    end

    # Count exterior surface
    exterior_sides = 0
    for cube in cubes
        for neighbor in adjacent_cubes(cube)
            if neighbor ∈ visited
                exterior_sides += 1
            end
        end
    end
    
    return exterior_sides
end

function main()
    cubes = read_input("input.txt")
    
    # Part 1
    println("Surface Area: ", surface_area(cubes))
    
    # Part 2
    println("Exterior Surface Area: ", exterior_surface_area(cubes))
end

main()
