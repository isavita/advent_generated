
using DataStructures

struct Position
    x::Int
    y::Int
    risk::Int
end

function Base.isless(a::Position, b::Position)
    return a.risk < b.risk
end

function dijkstra(grid)
    pq = PriorityQueue{Position, Int}()
    enqueue!(pq, Position(1, 1, 0), 0)

    rows, cols = size(grid)
    dist = fill(typemax(Int), rows, cols)
    dist[1, 1] = 0

    directions = [(1, 0), (0, 1), (-1, 0), (0, -1)]

    while !isempty(pq)
        curr = dequeue!(pq)
        if curr.x == rows && curr.y == cols
            return curr.risk
        end
        for (dx, dy) in directions
            nx, ny = curr.x + dx, curr.y + dy
            if nx >= 1 && ny >= 1 && nx <= rows && ny <= cols
                nextRisk = curr.risk + grid[nx, ny]
                if nextRisk < dist[nx, ny]
                    dist[nx, ny] = nextRisk
                    enqueue!(pq, Position(nx, ny, nextRisk), nextRisk)
                end
            end
        end
    end
    return -1
end

function extend_grid(initialGrid)
    rows, cols = size(initialGrid)
    extendedGrid = zeros(Int, rows * 5, cols * 5)
    for i in 1:(rows * 5)
        for j in 1:(cols * 5)
            newRisk = initialGrid[(i - 1) % rows + 1, (j - 1) % cols + 1] + div(i - 1, rows) + div(j - 1, cols)
            extendedGrid[i, j] = newRisk > 9 ? newRisk - 9 : newRisk
        end
    end
    return extendedGrid
end

function main()
    initialGrid = []
    open("input.txt") do file
        for line in eachline(file)
            push!(initialGrid, [parse(Int, ch) for ch in line])
        end
    end

    extendedGrid = extend_grid(hcat(initialGrid...))
    println(dijkstra(extendedGrid))
end

main()
