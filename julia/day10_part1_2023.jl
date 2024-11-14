
struct Coord
    x::Int
    y::Int
end

Base.:+(c1::Coord, c2::Coord) = Coord(c1.x + c2.x, c1.y + c2.y)
Base.:-(c1::Coord, c2::Coord) = Coord(c1.x - c2.x, c1.y - c2.y)
opposite(c::Coord) = Coord(-c.x, -c.y)

const Tile = Char
const Pipe = Dict{Coord,Nothing}

struct Grid
    width::Int
    height::Int
    data::Dict{Coord,Tile}
end

const Undefined = Coord(0, 0)
const Top = Coord(0, -1)
const Right = Coord(1, 0)
const Bottom = Coord(0, 1)
const Left = Coord(-1, 0)

const Empty = '.'
const Start = 'S'
const Vertical = '|'
const Horizontal = '-'
const TopLeftCorner = 'J'
const TopRightCorner = 'L'
const BottomLeftCorner = '7'
const BottomRightCorner = 'F'
const Enclosed = 'X'

const VerticalPipe = Dict(Top => nothing, Bottom => nothing)
const HorizontalPipe = Dict(Left => nothing, Right => nothing)
const TopLeftCornerPipe = Dict(Top => nothing, Left => nothing)
const TopRightCornerPipe = Dict(Top => nothing, Right => nothing)
const BottomLeftCornerPipe = Dict(Bottom => nothing, Left => nothing)
const BottomRightCornerPipe = Dict(Bottom => nothing, Right => nothing)

const TileToPipe = Dict(
    Vertical => VerticalPipe,
    Horizontal => HorizontalPipe,
    TopLeftCorner => TopLeftCornerPipe,
    TopRightCorner => TopRightCornerPipe,
    BottomLeftCorner => BottomLeftCornerPipe,
    BottomRightCorner => BottomRightCornerPipe
)

function getPipeFromTile(tile::Tile)::Pipe
    return get(TileToPipe, tile, Dict{Coord,Nothing}())
end

function getTileFromPipe(pipe::Pipe)::Tile
    for (tile, associatedPipe) in TileToPipe
        if pipe == associatedPipe
            return tile
        end
    end
    return Empty
end

function buildGrid(input::Vector{String})::Grid
    width = length(input[1])
    height = length(input)
    data = Dict{Coord,Tile}()
    for y in 1:height
        for x in 1:width
            char = input[y][x]
            if char != Empty
                data[Coord(x, y)] = char
            end
        end
    end
    return Grid(width, height, data)
end

function findStart(grid::Grid)::Coord
    for (coord, value) in grid.data
        if value == Start
            return coord
        end
    end
    return Undefined
end

function getPipeFromNeighbors(grid::Grid, c::Coord)::Pipe
    pipe = Dict{Coord,Nothing}()
    possibleNeighbors = Dict(Top => c + Top, Right => c + Right, Bottom => c + Bottom, Left => c + Left)
    for (dir, neighborCoord) in possibleNeighbors
        if haskey(grid.data, neighborCoord)
            neighborPipe = getPipeFromTile(grid.data[neighborCoord])
            if haskey(neighborPipe, opposite(dir))
                pipe[dir] = nothing
            end
        end
    end
    return pipe
end

function pathFinding(grid::Grid, start::Coord)::Vector{Coord}
    path = [start]
    startPipe = getPipeFromNeighbors(grid, start)
    previousDir = first(keys(startPipe))
    current = start + previousDir
    while current != start
        push!(path, current)
        currentPipe = getPipeFromTile(grid.data[current])
        for (dir, _) in currentPipe
            if dir != opposite(previousDir)
                previousDir = dir
                current += dir
                break
            end
        end
    end
    return path
end

function solve(input::Vector{String})::Int
    grid = buildGrid(input)
    start = findStart(grid)
    path = pathFinding(grid, start)
    numPipesVisited = length(path)
    maxLength = div(numPipesVisited, 2)
    return maxLength
end

input = readlines("input.txt")
println(solve(input))
