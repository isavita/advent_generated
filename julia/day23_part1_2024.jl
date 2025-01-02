
function solve()
    graph = Dict{String, Set{String}}()
    for line in eachline("input.txt")
        computers = split(line, "-")
        if length(computers) != 2
            continue
        end
        from, to = computers
        get!(graph, from, Set{String}())
        push!(graph[from], to)
        get!(graph, to, Set{String}())
        push!(graph[to], from)
    end

    triplets = Set{NTuple{3, String}}()
    nodes = collect(keys(graph))
    n = length(nodes)

    for i in 1:n
        for j in i+1:n
            for k in j+1:n
                c1, c2, c3 = nodes[i], nodes[j], nodes[k]
                if c2 in get(graph, c1, Set{String}()) &&
                   c3 in get(graph, c2, Set{String}()) &&
                   c3 in get(graph, c1, Set{String}())
                    if startswith(c1, "t") || startswith(c2, "t") || startswith(c3, "t")
                        push!(triplets, Tuple(sort([c1, c2, c3])))
                    end
                end
            end
        end
    end
    println("Number of triplets containing at least one computer with name starting with 't': ", length(triplets))
end

solve()
