
struct Coord
    x::Int
    y::Int
end

Base.:+(c1::Coord, c2::Coord) = Coord(c1.x + c2.x, c1.y + c2.y)

struct Grid
    width::Int
    height::Int
    data::Dict{Coord,Char}
end

isInBounds(coord::Coord, grid::Grid) = 0 <= coord.x < grid.width && 0 <= coord.y < grid.height

const Empty = '.'
const CubicRock = '#'
const RoundRock = 'O'

const North = Coord(0, -1)
const West = Coord(-1, 0)
const South = Coord(0, 1)
const East = Coord(1, 0)

function buildGrid(input::Vector{String})
    grid = Grid(length(input[1]), length(input), Dict{Coord,Char}())
    for (y, line) in enumerate(input)
        for (x, char) in enumerate(line)
            if char != Empty
                grid.data[Coord(x-1, y-1)] = char
            end
        end
    end
    return grid
end

function toString(grid::Grid)
    result = IOBuffer()
    for y in 0:grid.height-1
        for x in 0:grid.width-1
            coord = Coord(x, y)
            if haskey(grid.data, coord)
                print(result, grid.data[coord])
            else
                print(result, Empty)
            end
        end
        println(result)
    end
    return String(take!(result))
end

function shiftSingleRock!(grid::Grid, coord::Coord, dir::Coord)
    if get(grid.data, coord, Empty) == RoundRock
        current = coord
        before = current + dir
        while !haskey(grid.data, before) && isInBounds(before, grid)
            grid.data[before] = RoundRock
            delete!(grid.data, current)
            current = before
            before = before + dir
        end
    end
end

function shiftRocks!(grid::Grid, dir::Coord)
    if dir == North || dir == West
        for x in 0:grid.width-1
            for y in 0:grid.height-1
                shiftSingleRock!(grid, Coord(x, y), dir)
            end
        end
    else
        for x in grid.width-1:-1:0
            for y in grid.height-1:-1:0
                shiftSingleRock!(grid, Coord(x, y), dir)
            end
        end
    end
end

function cycleRocks!(grid::Grid)
    shiftRocks!(grid, North)
    shiftRocks!(grid, West)
    shiftRocks!(grid, South)
    shiftRocks!(grid, East)
end

function calculateGridKey(grid::Grid)
    key = 0
    for (coord, char) in grid.data
        if char == RoundRock
            key += coord.x + coord.y * grid.width
        end
    end
    return key
end

function calculateLoad(grid::Grid)
    load = 0
    for (coord, char) in grid.data
        if char == RoundRock
            load += grid.height - coord.y
        end
    end
    return load
end

function solve(input::Vector{String})
    numCycles = 1_000_000_000
    grid = buildGrid(input)
    cache = Dict{Int,Int}()
    for i in 0:numCycles-1
        gridKey = calculateGridKey(grid)
        if haskey(cache, gridKey)
            remainingCycles = (numCycles - i) % (i - cache[gridKey])
            for _ in 1:remainingCycles
                cycleRocks!(grid)
            end
            return calculateLoad(grid)
        end
        cache[gridKey] = i
        cycleRocks!(grid)
    end
    return calculateLoad(grid)
end

input = readlines("input.txt")
println(solve(input))
