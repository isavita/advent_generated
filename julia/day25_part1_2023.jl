using DataStructures

struct Vertice
    name::String
end

struct Edge
    start::Vertice
    finish::Vertice
    weight::Int
end

function parseInput(input::Vector{String})
    graph = Dict{Vertice,Dict{Edge,Bool}}()
    for line in input
        parts = split(line, ": ")
        vertice = Vertice(parts[1])
        others = split(parts[2])
        if !haskey(graph, vertice)
            graph[vertice] = Dict{Edge,Bool}()
        end
        for other in others
            otherVertice = Vertice(other)
            if !haskey(graph, otherVertice)
                graph[otherVertice] = Dict{Edge,Bool}()
            end
            edge1 = Edge(vertice, otherVertice, 1)
            edge2 = Edge(otherVertice, vertice, 1)
            graph[vertice][edge1] = true
            graph[otherVertice][edge2] = true
        end
    end
    return graph
end

function breadthFirstSearch(graph::Dict{Vertice,Dict{Edge,Bool}}, start::Vertice, goalFunc::Function)
    frontier = Queue{Vertice}()
    enqueue!(frontier, start)
    reached = Set{Vertice}([start])
    cameFrom = Dict{Vertice,Vertice}(start => start)
    while !isempty(frontier)
        current = dequeue!(frontier)
        if goalFunc(current)
            return true, cameFrom
        end
        for edge in keys(graph[current])
            if edge.finish ∉ reached
                enqueue!(frontier, edge.finish)
                push!(reached, edge.finish)
                cameFrom[edge.finish] = current
            end
        end
    end
    return false, cameFrom
end

function reconstructPath(start::Vertice, finish::Vertice, cameFrom::Dict{Vertice,Vertice})
    path = Vertice[]
    current = finish
    while current != start
        pushfirst!(path, current)
        current = cameFrom[current]
    end
    pushfirst!(path, start)
    return path
end

function copyGraph(graph::Dict{Vertice,Dict{Edge,Bool}})
    newGraph = Dict{Vertice,Dict{Edge,Bool}}()
    for (vertice, edges) in graph
        newGraph[vertice] = Dict{Edge,Bool}()
        for edge in keys(edges)
            newGraph[vertice][edge] = true
        end
    end
    return newGraph
end

function solve(input::Vector{String})
    minCut = 3
    graph = parseInput(input)
    source = first(keys(graph))
    separateGraph = nothing
    for currentVertex in keys(graph)
        if source == currentVertex
            continue
        end
        newGraph = copyGraph(graph)
        for _ in 1:minCut
            _, cameFrom = breadthFirstSearch(newGraph, source, x -> x == currentVertex)
            path = reconstructPath(source, currentVertex, cameFrom)
            for i in 1:length(path)-1
                edge = Edge(path[i], path[i+1], 1)
                delete!(newGraph[path[i]], edge)
            end
        end
        isValid, _ = breadthFirstSearch(newGraph, source, x -> x == currentVertex)
        if !isValid
            separateGraph = newGraph
            break
        end
    end
    _, cameFrom = breadthFirstSearch(separateGraph, source, _ -> false)
    length1 = length(cameFrom)
    length2 = length(separateGraph) - length1
    return length1 * length2
end

input = readlines("input.txt")
println(solve(input))