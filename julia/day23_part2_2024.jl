
function bron_kerbosch(R, P, X, graph, best_clique)
    if isempty(P) && isempty(X)
        if length(R) > length(best_clique[])
            best_clique[] = copy(R)
        end
        return
    end
    
    tempP = copy(P)
    for v in tempP
        neighbors = [n for n in keys(graph) if get(graph[v], n, false)]
        bron_kerbosch(
            union(R, [v]),
            intersect(P, neighbors),
            intersect(X, neighbors),
            graph,
            best_clique
        )
        P = setdiff(P, [v])
        X = union(X, [v])
    end
end

function main()
    graph = Dict{String, Dict{String, Bool}}()
    nodes_set = Set{String}()
    
    open("input.txt") do file
        for line in eachline(file)
            parts = split(line, "-")
            if length(parts) != 2
                continue
            end
            a, b = parts
            if !haskey(graph, a)
                graph[a] = Dict{String, Bool}()
            end
            if !haskey(graph, b)
                graph[b] = Dict{String, Bool}()
            end
            graph[a][b] = true
            graph[b][a] = true
            push!(nodes_set, a)
            push!(nodes_set, b)
        end
    end

    all_nodes = collect(nodes_set)
    best_clique = Ref{Vector{String}}([])
    bron_kerbosch(String[], all_nodes, String[], graph, best_clique)
    sort!(best_clique[])
    println(join(best_clique[], ","))
end

main()
