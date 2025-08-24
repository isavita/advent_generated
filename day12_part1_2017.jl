function read_graph(filename)
    graph = Dict{Int, Set{Int}}()
    open(filename, "r") do file
        for line in eachline(file)
            parts = split(line, " <-> ")
            node = parse(Int, parts[1])
            connected_nodes = Set(parse.(Int, split(parts[2], ", ")))
            graph[node] = connected_nodes
            for connected_node in connected_nodes
                if haskey(graph, connected_node)
                    push!(graph[connected_node], node)
                else
                    graph[connected_node] = Set([node])
                end
            end
        end
    end
    return graph
end

function count_group_members(graph, start_node)
    visited = Set{Int}()
    queue = [start_node]

    while !isempty(queue)
        node = popfirst!(queue)
        if !in(node, visited)
            push!(visited, node)
            for neighbor in graph[node]
                if !in(neighbor, visited)
                    push!(queue, neighbor)
                end
            end
        end
    end

    return length(visited)
end

function main()
    graph = read_graph("input.txt")
    group_size = count_group_members(graph, 0)
    println("Number of programs in the group containing program ID 0: $group_size")
end

main()