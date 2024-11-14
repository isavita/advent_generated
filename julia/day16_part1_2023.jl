
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

struct Beam
    origin::Coord
    dir::Coord
end

const EMPTY = '.'
const ASCENDING_MIRROR = '/'
const DESCENDING_MIRROR = '\\'
const VERTICAL_SPLITTER = '|'
const HORIZONTAL_SPLITTER = '-'

const NORTH = Coord(0, -1)
const WEST = Coord(-1, 0)
const SOUTH = Coord(0, 1)
const EAST = Coord(1, 0)

rotate90(coord::Coord) = Coord(coord.y, -coord.x)
rotateNeg90(coord::Coord) = Coord(-coord.y, coord.x)

isInBounds(coord::Coord, grid::Grid) = 0 <= coord.x < grid.width && 0 <= coord.y < grid.height

function buildGrid(input::Vector{String})
    width = length(input[1])
    height = length(input)
    data = Dict{Coord,Char}()
    for (y, line) in enumerate(input)
        for (x, char) in enumerate(line)
            if char != EMPTY
                data[Coord(x-1, y-1)] = char
            end
        end
    end
    Grid(width, height, data)
end

function nextBeam(grid::Grid, beam::Beam)
    beams = Beam[]
    char = get(grid.data, beam.origin, EMPTY)

    if char == EMPTY
        push!(beams, Beam(beam.origin + beam.dir, beam.dir))
    elseif char == ASCENDING_MIRROR
        newDir = beam.dir == NORTH || beam.dir == SOUTH ? rotateNeg90(beam.dir) : rotate90(beam.dir)
        push!(beams, Beam(beam.origin + newDir, newDir))
    elseif char == DESCENDING_MIRROR
        newDir = beam.dir == NORTH || beam.dir == SOUTH ? rotate90(beam.dir) : rotateNeg90(beam.dir)
        push!(beams, Beam(beam.origin + newDir, newDir))
    elseif char == VERTICAL_SPLITTER && (beam.dir == EAST || beam.dir == WEST)
        newDir1, newDir2 = rotate90(beam.dir), rotateNeg90(beam.dir)
        push!(beams, Beam(beam.origin + newDir1, newDir1), Beam(beam.origin + newDir2, newDir2))
    elseif char == HORIZONTAL_SPLITTER && (beam.dir == NORTH || beam.dir == SOUTH)
        newDir1, newDir2 = rotate90(beam.dir), rotateNeg90(beam.dir)
        push!(beams, Beam(beam.origin + newDir1, newDir1), Beam(beam.origin + newDir2, newDir2))
    else
        push!(beams, Beam(beam.origin + beam.dir, beam.dir))
    end
    beams
end

function calculatePropagation(grid::Grid, start::Beam)
    alreadySeen = Dict{Beam,Nothing}()
    toExplore = [start]
    while !isempty(toExplore)
        beam = popfirst!(toExplore)
        if isInBounds(beam.origin, grid) && !haskey(alreadySeen, beam)
            alreadySeen[beam] = nothing
            append!(toExplore, nextBeam(grid, beam))
        end
    end
    alreadySeen
end

function calculateEnergization(alreadySeen::Dict{Beam,Nothing})
    alreadyEnergized = Dict{Coord,Nothing}()
    for beam in keys(alreadySeen)
        alreadyEnergized[beam.origin] = nothing
    end
    alreadyEnergized
end

function getBorder(grid::Grid)
    border = Beam[]
    for x in 0:grid.width-1
        push!(border, Beam(Coord(x, 0), SOUTH))
        push!(border, Beam(Coord(x, grid.height-1), NORTH))
    end
    for y in 0:grid.height-1
        push!(border, Beam(Coord(0, y), EAST))
        push!(border, Beam(Coord(grid.width-1, y), WEST))
    end
    border
end

function solve(input::Vector{String})
    grid = buildGrid(input)
    start = Beam(Coord(0, 0), EAST)
    alreadySeen = calculatePropagation(grid, start)
    alreadyEnergized = calculateEnergization(alreadySeen)
    length(alreadyEnergized)
end

input = readlines("input.txt")
println(solve(input))
