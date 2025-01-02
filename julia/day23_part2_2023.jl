
struct Coord
    x::Int
    y::Int
end

Base.:+(c1::Coord, c2::Coord) = Coord(c1.x + c2.x, c1.y + c2.y)

struct Grid
    width::Int
    height::Int
    data::Dict{Coord, Char}
end

const North = Coord(0, -1)
const South = Coord(0, 1)
const West = Coord(-1, 0)
const East = Coord(1, 0)

const Empty = '.'
const Wall = '#'
const NorthSlopes = '^'
const SouthSlopes = 'v'
const WestSlopes = '<'
const EastSlopes = '>'

const SlopeToDir = Dict(
    NorthSlopes => North,
    SouthSlopes => South,
    WestSlopes => West,
    EastSlopes => East,
)

struct Edge
    start::Coord
    end_::Coord
    weight::Int
end

struct Graph
    vertices::Set{Coord}
    edges::Dict{Coord, Set{Edge}}
end

function is_in_bounds(grid::Grid, coord::Coord)
    0 <= coord.x < grid.width && 0 <= coord.y < grid.height
end

function parse_input(input::Vector{String})
    grid = Grid(
        length(input[1]),
        length(input),
        Dict{Coord, Char}(),
    )

    for (y, line) in enumerate(input)
        for (x, char) in enumerate(line)
            if char != Empty
                grid.data[Coord(x - 1, y - 1)] = char
            end
        end
    end

    grid
end

function is_valid_neighbor(grid::Grid, coord::Coord, dir::Coord)
    !is_in_bounds(grid, coord) && return false
    get(grid.data, coord, Empty) == Wall && return false
    true
end

function neighbors4(grid::Grid, coord::Coord, is_valid_neighbor_func::Function)
    directions = [North, South, West, East]
    valid_neighbors = Coord[]

    for dir in directions
        neighbor = coord + dir
        if is_valid_neighbor_func(grid, neighbor, dir)
            push!(valid_neighbors, neighbor)
        end
    end

    valid_neighbors
end

function get_graph(grid::Grid, start::Coord, end_::Coord, is_valid_neighbor_func::Function)
    graph = Graph(
        Set([start, end_]),
        Dict{Coord, Set{Edge}}(),
    )

    for y in 0:grid.height-1
        for x in 0:grid.width-1
            coord = Coord(x, y)
            if get(grid.data, coord, Empty) == Empty
                if length(neighbors4(grid, coord, is_valid_neighbor)) > 2
                    push!(graph.vertices, coord)
                end
            end
        end
    end

    for start_ in graph.vertices
        graph.edges[start_] = get_edges_bfs(grid, start_, graph.vertices, is_valid_neighbor_func)
    end

    graph
end

function get_edges_bfs(grid::Grid, start::Coord, vertices::Set{Coord}, is_valid_neighbor_func::Function)
    frontier = [start]
    reached = Set([start])
    distances = Dict(start => 0)
    edges = Set{Edge}()

    while !isempty(frontier)
        current = popfirst!(frontier)

        if current in vertices && current != start
            push!(edges, Edge(start, current, distances[current]))
            continue
        end

        for next_ in neighbors4(grid, current, is_valid_neighbor_func)
            if !(next_ in reached)
                push!(frontier, next_)
                push!(reached, next_)
                distances[next_] = distances[current] + 1
            end
        end
    end

    edges
end

function get_max_distance_dfs(grid::Grid, graph::Graph, current::Coord, end_::Coord, seen::Set{Coord})
    if current == end_
        return true, 0
    end

    maxi = 0
    push!(seen, current)
    for edge in graph.edges[current]
        if !(edge.end_ in seen)
            is_valid, dist = get_max_distance_dfs(grid, graph, edge.end_, end_, seen)
            if is_valid
                maxi = max(maxi, dist + edge.weight)
            end
        end
    end
    delete!(seen, current)

    maxi == 0 && return false, 0
    true, maxi
end

function solve(input::Vector{String})
    grid = parse_input(input)

    start = Coord(1, 0)
    end_ = Coord(grid.width - 2, grid.height - 1)

    graph = get_graph(grid, start, end_, is_valid_neighbor)

    _, max_dist = get_max_distance_dfs(grid, graph, start, end_, Set{Coord}())
    max_dist
end

function read_file(file_name::String)
    readlines(file_name)
end

input = read_file("input.txt")
println(solve(input))
