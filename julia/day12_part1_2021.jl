using DataStructures

function solve(filename)
    graph = Dict{String,Vector{String}}()
    for line in readlines(filename)
        a,b = split(line,"-")
        if !haskey(graph,a)
            graph[a] = String[]
        end
        if !haskey(graph,b)
            graph[b] = String[]
        end
        push!(graph[a],b)
        push!(graph[b],a)
    end
    global paths = 0 # Define paths as a global variable
    dfs(graph,"start",Set(["start"]),paths)
    return paths
end

function dfs(graph,node,visited,paths)
    if node=="end"
        global paths += 1 # Increment the global paths variable
        return
    end
    for neighbor in graph[node]
        if neighbor=="start"
            continue
        elseif islowercase(neighbor[1]) && neighbor in visited
            continue
        else
            new_visited = copy(visited)
            push!(new_visited,neighbor)
            dfs(graph,neighbor,new_visited,paths)
        end
    end
end

println(solve("input.txt"))