
struct Coord
    x::Int
    y::Int
end

Base.:+(c1::Coord, c2::Coord) = Coord(c1.x + c2.x, c1.y + c2.y)
Base.:-(c1::Coord, c2::Coord) = Coord(c1.x - c2.x, c1.y - c2.y)
Base.:-(c::Coord) = Coord(-c.x, -c.y)

const Tile = Char
const Pipe = Set{Coord}

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

const VerticalPipe = Set([Top, Bottom])
const HorizontalPipe = Set([Left, Right])
const TopLeftCornerPipe = Set([Top, Left])
const TopRightCornerPipe = Set([Top, Right])
const BottomLeftCornerPipe = Set([Bottom, Left])
const BottomRightCornerPipe = Set([Bottom, Right])

const TileToPipe = Dict(
    Vertical => VerticalPipe,
    Horizontal => HorizontalPipe,
    TopLeftCorner => TopLeftCornerPipe,
    TopRightCorner => TopRightCornerPipe,
    BottomLeftCorner => BottomLeftCornerPipe,
    BottomRightCorner => BottomRightCornerPipe,
)

getPipeFromTile(tile::Tile) = get(TileToPipe, tile, Set{Coord}())

function getTileFromPipe(pipe::Pipe)
    for (tile, associatedPipe) in TileToPipe
        if pipe == associatedPipe
            return tile
        end
    end
    return Empty
end

function buildGrid(input::Vector{String})
    grid = Grid(length(input[1]), length(input), Dict{Coord,Tile}())
    for (y, line) in enumerate(input)
        for (x, char) in enumerate(line)
            if Tile(char) != Empty
                grid.data[Coord(x, y)] = Tile(char)
            end
        end
    end
    return grid
end

function findStart(grid::Grid)
    for (coord, value) in grid.data
        if value == Start
            return coord
        end
    end
    return Coord(0, 0)
end

function getPipeFromNeighbors(c::Coord, grid::Grid)
    pipe = Set{Coord}()
    possibleNeighbors = Dict(
        Top => c + Top,
        Right => c + Right,
        Bottom => c + Bottom,
        Left => c + Left,
    )
    for dir in keys(possibleNeighbors)
        neighborCoord = possibleNeighbors[dir]
        neighborPipe = getPipeFromTile(get(grid.data, neighborCoord, Empty))
        if -dir in neighborPipe
            push!(pipe, dir)
        end
    end
    return pipe
end

function pathFinding(start::Coord, grid::Grid)
    path = [start]
    startPipe = getPipeFromNeighbors(start, grid)
    previousDir = first(startPipe)
    current = start + previousDir

    while current != start
        push!(path, current)
        currentPipe = getPipeFromTile(grid.data[current])
        for dir in currentPipe
            if dir != -previousDir
                previousDir = dir
                current = current + dir
                break
            end
        end
    end
    return path
end

function getPathGrid(grid::Grid, path::Vector{Coord}, empty::Tile)
    newGrid = Grid(grid.width, grid.height, Dict{Coord,Tile}())
    for coord in path
        newGrid.data[coord] = grid.data[coord]
    end
    start = path[1]
    newGrid.data[start] = getTileFromPipe(getPipeFromNeighbors(start, grid))
    return newGrid
end

function isInside(c::Coord, grid::Grid, empty::Tile)
    haskey(grid.data, c) && return false
    startPipe = empty
    numPipeOnLeft = 0
    for x = 1:c.x-1
        coord = Coord(x, c.y)
        v = get(grid.data, coord, Empty)
        if v == Vertical
            numPipeOnLeft += 1
        elseif v == TopRightCorner
            startPipe = TopRightCorner
        elseif v == BottomRightCorner
            startPipe = BottomRightCorner
        elseif v == TopLeftCorner
            if startPipe == BottomRightCorner
                startPipe = empty
                numPipeOnLeft += 1
            elseif startPipe == TopRightCorner
                startPipe = Empty
            end
        elseif v == BottomLeftCorner
            if startPipe == TopRightCorner
                startPipe = Empty
                numPipeOnLeft += 1
            elseif startPipe == BottomRightCorner
                startPipe = Empty
            end
        end
    end
    return numPipeOnLeft % 2 == 1
end

function solve(input::Vector{String})
    grid = buildGrid(input)
    start = findStart(grid)
    path = pathFinding(start, grid)
    pathGrid = getPathGrid(grid, path, Empty)
    cnt = 0
    for y = 1:grid.height
        for x = 1:grid.width
            c = Coord(x, y)
            if isInside(c, pathGrid, Empty)
                cnt += 1
            end
        end
    end
    return cnt
end

function readFile(fileName::String)
    return readlines(fileName)
end

input = readFile("input.txt")
println(solve(input))
