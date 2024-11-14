using DataStructures

function read_input(filename)
    open(filename, "r") do file
        return [strip(line) for line in readlines(file)]
    end
end

function parse_grid(lines)
    grid = Dict{Tuple{Int, Int}, Char}()
    start, end_point = (0, 0), (0, 0)
    as = Tuple{Int, Int}[]
    for (y, line) in enumerate(lines)
        for (x, char) in enumerate(line)
            if char == 'S'
                start = (x, y)
            elseif char == 'E'
                end_point = (x, y)
            elseif char == 'a'
                push!(as, (x, y))
            end
            grid[(x, y)] = char
        end
    end
    grid[start], grid[end_point] = 'a', 'z'
    return grid, start, end_point, as
end

function dijkstra(grid, end_point)
    pq = PriorityQueue{Tuple{Int, Int}, Int}()
    dist = Dict{Tuple{Int, Int}, Int}()
    dist[end_point] = 0
    enqueue!(pq, end_point => 0)
    neighbors = [(0, 1), (0, -1), (1, 0), (-1, 0)]
    while !isempty(pq)
        curr = dequeue!(pq)
        for n in neighbors
            next = (curr[1] + n[1], curr[2] + n[2])
            if !haskey(grid, next) || Int(grid[curr]) - Int(grid[next]) > 1
                continue
            end
            nextdist = dist[curr] + 1
            if !haskey(dist, next) || nextdist < dist[next]
                dist[next] = nextdist
                enqueue!(pq, next => nextdist)
            end
        end
    end
    return dist
end

function main()
    lines = read_input("input.txt")
    grid, start, end_point, as = parse_grid(lines)
    dists = dijkstra(grid, end_point)
    println(dists[start])
end

main()