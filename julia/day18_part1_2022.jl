
struct Point
    x::Int
    y::Int
    z::Int
end

function main()
    cubes = Dict{Point, Bool}()

    for line in eachline("input.txt")
        coords = split(line, ",")
        x = parse(Int, coords[1])
        y = parse(Int, coords[2])
        z = parse(Int, coords[3])
        cubes[Point(x, y, z)] = true
    end

    surfaceArea = 0
    for cube in keys(cubes)
        surfaceArea += calculateExposedSides(cube, cubes)
    end

    println(surfaceArea)
end

function calculateExposedSides(p::Point, cubes::Dict{Point, Bool})
    directions = [Point(1, 0, 0), Point(-1, 0, 0), Point(0, 1, 0), Point(0, -1, 0), Point(0, 0, 1), Point(0, 0, -1)]

    exposedSides = 6
    for dir in directions
        adjacent = Point(p.x + dir.x, p.y + dir.y, p.z + dir.z)
        if haskey(cubes, adjacent)
            exposedSides -= 1
        end
    end
    return exposedSides
end

main()
