
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

function parse_input(input::Vector{String})
    current = Coord(0, 0)
    vertices = [current]

    for line in input
        parts = split(line)
        color = parts[3]
        dir_input = color[8]
        length_str = color[3:7]
        length = parse(Int, length_str, base=16)

        dir = if dir_input == '3'
            North
        elseif dir_input == '2'
            West
        elseif dir_input == '1'
            South
        else
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

    @inbounds for i in 1:n
        next = i % n + 1
        area += vertices[i].x * vertices[next].y
        area -= vertices[i].y * vertices[next].x
    end

    return abs(area) ÷ 2
end

function perimeter(vertices::Vector{Coord})
    n = length(vertices)
    perim = 0

    @inbounds for i in 1:n
        next = i % n + 1
        perim += abs(vertices[i].x - vertices[next].x) + abs(vertices[i].y - vertices[next].y)
    end

    return perim
end

function calculate_polygon_area(vertices::Vector{Coord})
    return shoelace(vertices) + perimeter(vertices) ÷ 2 + 1
end

function solve(input::Vector{String})
    vertices = parse_input(input)
    return calculate_polygon_area(vertices)
end

function main()
    input = readlines("input.txt")
    println(solve(input))
end

main()
