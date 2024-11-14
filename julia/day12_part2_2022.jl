
using DataStructures

function main()
    grid = Dict{Tuple{Int,Int},Char}()
    start, endp = (0, 0), (0, 0)
    as = Tuple{Int,Int}[]
    y = 0
    open("input.txt", "r") do file
        for line in eachline(file)
            for (x, b) in enumerate(line)
                p = (x, y)
                grid[p] = b
                if b == 'S'
                    start = p
                elseif b == 'E'
                    endp = p
                elseif b == 'a'
                    push!(as, p)
                end
            end
            y += 1
        end
    end
    grid[start], grid[endp] = 'a', 'z'

    dists = dijkstra(grid, endp)

    l = dists[start]

    for a in as
        if haskey(dists, a)
            l = min(l, dists[a])
        end
    end
    println(l)
end

function dijkstra(grid::Dict{Tuple{Int,Int},Char}, endp::Tuple{Int,Int})
    pq = PriorityQueue{Tuple{Int,Int},Int}()
    dist = Dict{Tuple{Int,Int},Int}(endp => 0)
    enqueue!(pq, endp, 0)
    neighbors4 = [(0, 1), (0, -1), (1, 0), (-1, 0)]
    while !isempty(pq)
        curr = dequeue!(pq)
        for n in neighbors4
            next = (curr[1] + n[1], curr[2] + n[2])
            if !haskey(grid, next)
                continue
            end
            if Int(grid[curr]) - Int(grid[next]) > 1
                continue
            end
            nextdist = dist[curr] + 1
            if !haskey(dist, next) || nextdist < dist[next]
                dist[next] = nextdist
                enqueue!(pq, next, nextdist)
            end
        end
    end
    return dist
end

main()
