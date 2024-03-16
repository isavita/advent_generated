struct Coord
    x::Int
    y::Int
end

function add(c1::Coord, c2::Coord)
    return Coord(c1.x + c2.x, c1.y + c2.y)
end

struct Grid
    width::Int
    height::Int
    data::Dict{Coord,Char}
end

function is_in_bounds(coord::Coord, grid::Grid)
    return 0 <= coord.x < grid.width && 0 <= coord.y < grid.height
end

const Empty = '.'
const CubicRock = '#'
const RoundRock = 'O'

const North = Coord(0, -1)
const West = Coord(-1, 0)
const South = Coord(0, 1)
const East = Coord(1, 0)

function build_grid(input::Vector{String})
    width = length(input[1])
    height = length(input)
    data = Dict{Coord,Char}()

    for y in 1:height
        for x in 1:width
            if input[y][x] != Empty
                data[Coord(x-1, y-1)] = input[y][x]
            end
        end
    end

    return Grid(width, height, data)
end

function to_string(grid::Grid)
    result = ""
    for y in 0:grid.height-1
        for x in 0:grid.width-1
            coord = Coord(x, y)
            if haskey(grid.data, coord)
                result *= grid.data[coord]
            else
                result *= Empty
            end
        end
        result *= "\n"
    end
    return result
end

function shift_single_rock(grid::Grid, coord::Coord, dir::Coord)
    if haskey(grid.data, coord) && grid.data[coord] == RoundRock
        current = coord
        before = add(coord, dir)

        while is_in_bounds(before, grid) && !haskey(grid.data, before)
            grid.data[before] = RoundRock
            delete!(grid.data, current)

            current = before
            before = add(before, dir)
        end
    end
end

function shift_rocks(grid::Grid, dir::Coord)
    if dir == North || dir == West
        for y in 0:grid.height-1
            for x in 0:grid.width-1
                shift_single_rock(grid, Coord(x, y), dir)
            end
        end
    else
        for y in grid.height-1:-1:0
            for x in grid.width-1:-1:0
                shift_single_rock(grid, Coord(x, y), dir)
            end
        end
    end
end

function calculate_load(grid::Grid)
    load = 0
    for x in 0:grid.width-1
        for y in 0:grid.height-1
            coord = Coord(x, y)
            if haskey(grid.data, coord) && grid.data[coord] == RoundRock
                load += grid.height - y
            end
        end
    end
    return load
end

function solve(input::Vector{String})
    grid = build_grid(input)
    shift_rocks(grid, North)
    return calculate_load(grid)
end

function read_file(file_name::String)
    return readlines(file_name)
end

input = read_file("input.txt")
println(solve(input))