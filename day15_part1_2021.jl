using DataStructures

mutable struct Position
    x::Int
    y::Int
    risk::Int
end

Base.isless(a::Position, b::Position) = a.risk < b.risk

function dijkstra(grid::Vector{Vector{Int}})
    pq = BinaryMinHeap{Position}()
    push!(pq, Position(1, 1, 0))

    rows = length(grid)
    cols = length(grid[1])
    dist = fill(typemax(Int), rows, cols)
    dist[1, 1] = 0

    directions = [Position(1, 0, 0), Position(0, 1, 0), Position(-1, 0, 0), Position(0, -1, 0)]

    while !isempty(pq)
        curr = pop!(pq)
        if curr.x == rows && curr.y == cols
            return curr.risk
        end
        for d in directions
            nx, ny = curr.x + d.x, curr.y + d.y
            if 1 <= nx <= rows && 1 <= ny <= cols
                nextRisk = curr.risk + grid[nx][ny]
                if nextRisk < dist[nx, ny]
                    dist[nx, ny] = nextRisk
                    push!(pq, Position(nx, ny, nextRisk))
                end
            end
        end
    end
    return -1
end

grid = Vector{Vector{Int}}()
open("input.txt") do file
    for line in eachline(file)
        row = [parse(Int, ch) for ch in line]
        push!(grid, row)
    end
end

println(dijkstra(grid))