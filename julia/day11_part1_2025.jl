using Base.Iterators: flatten

const START_NODE = "you"
const END_NODE = "out"

function parse_input(filename)
    adj = Dict{String,Vector{String}}()
    open(filename) do io
        for line in eachline(io)
            line = strip(line)
            isempty(line) && continue
            parts = split(line, ":", limit=2)
            src = strip(parts[1])
            targets = String[]
            if length(parts) == 2
                for t in split(strip(parts[2]))
                    !isempty(t) && push!(targets, t)
                end
            end
            adj[src] = targets
        end
    end
    adj
end

function count_paths(node, adj, memo)
    node == END_NODE && return 1
    haskey(memo, node) && return memo[node]
    total = 0
    for nb in get(adj, node, String[])
        total += count_paths(nb, adj, memo)
    end
    memo[node] = total
    total
end

function main()
    adj = parse_input("input.txt")
    memo = Dict{String,Int64}()
    println(count_paths(START_NODE, adj, memo))
end

main()
