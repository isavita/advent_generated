struct Coord
    x::Int
    y::Int
end

# Overload the + operator for Coord
Base.:+(c1::Coord, c2::Coord) = Coord(c1.x + c2.x, c1.y + c2.y)

function *(c::Coord, s::Int)
    return Coord(c.x * s, c.y * s)
end

struct Grid
    width::Int
    height::Int
    data::Dict{Coord,Char}
end

const North = Coord(0, -1)
const West = Coord(-1, 0)
const South = Coord(0, 1)
const East = Coord(1, 0)

const Empty = '.'
const Rock = '#'
const Start = 'S'

function to_string(grid::Grid)
    res = ""
    for y in 0:(grid.height-1)
        for x in 0:(grid.width-1)
            c = Coord(x, y)
            if haskey(grid.data, c)
                res *= grid.data[c]
            else
                res *= string(Empty)
            end
        end
        res *= "\n"
    end
    return res
end

function is_in_bounds(grid::Grid, c::Coord)
    return 0 <= c.x < grid.width && 0 <= c.y < grid.height
end

function parse_input(input::Vector{String})
    grid = Grid(
        length(input[1]),
        length(input),
        Dict{Coord,Char}()
    )
    for y in 1:grid.height
        for x in 1:grid.width
            c = Coord(x-1, y-1)
            grid.data[c] = input[y][x]
        end
    end
    return grid
end

function find_start(grid::Grid)
    for c in keys(grid.data)
        if grid.data[c] == Start
            return c
        end
    end
    error("No start found.")
end

function neighbors4(grid::Grid, c::Coord)
    neighbors = [c + North, c + South, c + East, c + West]
    return filter(n -> is_in_bounds(grid, n) && grid.data[n] != Rock, neighbors)
end

function breadth_first_search(grid::Grid, start::Coord, neighbor_func::Function)
    frontier = [start]
    reached = Set([start])
    came_from = Dict(start => start)
    distances = Dict(start => 0)

    while !isempty(frontier)
        current = popfirst!(frontier)
        for next in neighbor_func(grid, current)
            if !(next in reached)
                push!(frontier, next)
                push!(reached, next)
                came_from[next] = current
                distances[next] = distances[current] + 1
            end
        end
    end

    return distances
end

function distances_from_extremities(grid::Grid)
    distances = Dict{Coord,Dict{Coord,Int}}()

    extremities = [
        Coord(0, 0),
        Coord(grid.width ÷ 2, 0),
        Coord(grid.width, 0),
        Coord(grid.width, grid.height ÷ 2),
        Coord(grid.width, grid.height),
        Coord(grid.width ÷ 2, grid.height),
        Coord(0, grid.height),
        Coord(0, grid.height ÷ 2)
    ]

    for start in extremities
        distances[start] = breadth_first_search(grid, start, neighbors4)
    end

    return distances
end

function neighbors8(grid::Grid, c::Coord)
    neighbors = [
        c + North, c + South, c + East, c + West,
        c + (North + East), c + (North + West), c + (South + East), c + (South + West)
    ]
    return filter(n -> is_in_bounds(grid, n), neighbors)
end

function solve(input::Vector{String}, num_steps::Int)
    grid = parse_input(input)
    start = find_start(grid)
    distances = breadth_first_search(grid, start, neighbors4)

    count = 0
    for dist in values(distances)
        if dist <= num_steps && dist % 2 == 0
            count += 1
        end
    end
    return count
end

function read_file(file_name::String)
    return readlines(file_name)
end

input = read_file("input.txt")
println(solve(input, 64))