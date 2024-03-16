using Dates

struct Coordinate
    x::Int
    y::Int
    z::Int
end

function simulate_cycle(active_cubes)
    new_active_cubes = Set{Coordinate}()
    neighbor_counts = Dict{Coordinate,Int}()

    for cube in active_cubes
        for dz in -1:1, dy in -1:1, dx in -1:1
            if dz == 0 && dy == 0 && dx == 0
                continue
            end
            neighbor = Coordinate(cube.x + dx, cube.y + dy, cube.z + dz)
            neighbor_counts[neighbor] = get(neighbor_counts, neighbor, 0) + 1
        end
    end

    for (neighbor, count) in neighbor_counts
        if count == 3 || (count == 2 && neighbor in active_cubes)
            push!(new_active_cubes, neighbor)
        end
    end

    return new_active_cubes
end

function main()
    input = read("input.txt", String)
    initial_state = split(strip(input), "\n")
    active_cubes = Set{Coordinate}()

    for y in 1:length(initial_state), x in 1:length(initial_state[y])
        if initial_state[y][x] == '#'
            push!(active_cubes, Coordinate(x-1, y-1, 0))
        end
    end

    for cycle in 1:6
        active_cubes = simulate_cycle(active_cubes)
    end

    println(length(active_cubes))
end

main()