using DataStructures

function cleaningRobot(input)
    grid = [collect(l) for l in split(input, "\n")]
    graph = nothing
    for (r, row) in enumerate(grid)
        for (c, cell) in enumerate(row)
            if occursin(r"[0-9]", string(cell))
                poi = cell
                distancesFromPOI = bfsGetEdgeWeights(grid, (r, c))
                if graph === nothing
                    graph = [zeros(Int, length(distancesFromPOI)) for _ in 1:length(distancesFromPOI)]
                end
                index = parse(Int, poi)
                graph[index+1] = distancesFromPOI
            end
        end
    end
    return dfs(graph, 1, Dict(1 => true), true)
end

struct BFSNode
    row::Int
    col::Int
    distance::Int
end

function bfsGetEdgeWeights(grid, start)
    poiToDistance = Dict(string(grid[start[1]][start[2]]) => 0)
    queue = [BFSNode(start[1], start[2], 0)]
    visited = Set()
    while !isempty(queue)
        front = popfirst!(queue)
        if (front.row, front.col) in visited
            continue
        end
        push!(visited, (front.row, front.col))
        if occursin(r"[0-9]", string(grid[front.row][front.col]))
            poiToDistance[string(grid[front.row][front.col])] = front.distance
        end
        for d in dirs
            nextRow, nextCol = front.row + d[1], front.col + d[2]
            if 1 <= nextRow <= size(grid, 1) && 1 <= nextCol <= size(grid[1], 1) && grid[nextRow][nextCol] != '#'
                push!(queue, BFSNode(nextRow, nextCol, front.distance + 1))
            end
        end
    end
    distances = zeros(Int, maximum(parse.(Int, keys(poiToDistance))) + 1)
    for (numStr, dist) in poiToDistance
        n = parse(Int, numStr)
        distances[n+1] = dist
    end
    return distances
end

const dirs = [(0, -1), (0, 1), (1, 0), (-1, 0)]

function dfs(graph, entryIndex, visited, returnToZero)
    if length(graph) == length(visited)
        if returnToZero
            return graph[entryIndex][1]
        end
        return 0
    end
    minDistance = typemax(Int)
    for (i, val) in enumerate(graph[entryIndex])
        if !haskey(visited, i)
            visited[i] = true
            dist = val + dfs(graph, i, visited, returnToZero)
            minDistance = min(minDistance, dist)
            delete!(visited, i)
        end
    end
    return minDistance
end

input = read("input.txt", String)
n = cleaningRobot(input)
println(n)