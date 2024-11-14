
struct Coord
    x::Int
    y::Int
end

Base.:+(c1::Coord, c2::Coord) = Coord(c1.x + c2.x, c1.y + c2.y)
Base.:*(c::Coord, s::Int) = Coord(c.x * s, c.y * s)

const North = Coord(0, -1)
const West = Coord(-1, 0)
const South = Coord(0, 1)
const East = Coord(1, 0)

abs(x::Int) = x < 0 ? -x : x

function parseInput(input::Vector{String})
    current = Coord(0, 0)
    vertices = [current]

    for line in input
        parts = split(line)
        dirInput = parts[1][1]
        length = parse(Int, parts[2])

        dir = if dirInput == 'U'
            North
        elseif dirInput == 'L'
            West
        elseif dirInput == 'D'
            South
        elseif dirInput == 'R'
            East
        end

        current += dir * length
        push!(vertices, current)
    end

    return vertices
end

function shoelace(vertices::Vector{Coord})
    n = length(vertices)
    area = 0

    for i in 1:n
        next = i % n + 1
        area += vertices[i].x * vertices[next].y - vertices[i].y * vertices[next].x
    end

    return abs(area) ÷ 2
end

function perimeter(vertices::Vector{Coord})
    n = length(vertices)
    perim = 0

    for i in 1:n
        next = i % n + 1
        perim += abs(vertices[i].x - vertices[next].x) + abs(vertices[i].y - vertices[next].y)
    end

    return perim
end

function calculatePolygonArea(vertices::Vector{Coord})
    return shoelace(vertices) + perimeter(vertices) ÷ 2 + 1
end

function solve(input::Vector{String})
    vertices = parseInput(input)
    return calculatePolygonArea(vertices)
end

input = readlines("input.txt")
println(solve(input))
