
using Printf

struct Coordinate4D
    x::Int
    y::Int
    z::Int
    w::Int
end

function main()
    input = read("input.txt", String)
    initialState = split(strip(input), '\n')
    activeCubes = Dict{Coordinate4D, Bool}()

    for (y, line) in enumerate(initialState)
        for (x, char) in enumerate(line)
            if char == '#'
                activeCubes[Coordinate4D(x, y, 0, 0)] = true
            end
        end
    end

    for cycle in 1:6
        activeCubes = simulateCycle4D(activeCubes)
    end

    @printf("%d\n", length(activeCubes))
end

function simulateCycle4D(activeCubes)
    newActiveCubes = Dict{Coordinate4D, Bool}()
    neighborCounts = Dict{Coordinate4D, Int}()

    for coord in keys(activeCubes)
        for dw in -1:1
            for dz in -1:1
                for dy in -1:1
                    for dx in -1:1
                        if dw == 0 && dz == 0 && dy == 0 && dx == 0
                            continue
                        end
                        neighbor = Coordinate4D(coord.x + dx, coord.y + dy, coord.z + dz, coord.w + dw)
                        neighborCounts[neighbor] = get(neighborCounts, neighbor, 0) + 1
                    end
                end
            end
        end
    end

    for (coord, count) in neighborCounts
        if count == 3 || (count == 2 && get(activeCubes, coord, false))
            newActiveCubes[coord] = true
        end
    end

    return newActiveCubes
end

main()
