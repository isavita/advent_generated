
function count_paths(adj, memo, cur, target)
    cur == target && return 1
    key = cur * "->" * target
    haskey(memo, key) && return memo[key]
    total = 0
    if haskey(adj, cur)
        for nxt in adj[cur]
            total += count_paths(adj, memo, nxt, target)
        end
    end
    memo[key] = total
    total
end

function main()
    filename = "input.txt"
    isfile(filename) || return
    adj = Dict{String,Vector{String}}()
    open(filename) do io
        for line in eachline(io)
            line = strip(line)
            isempty(line) && continue
            occursin(":", line) || continue
            parts = split(line, ":", limit=2)
            node = strip(parts[1])
            targets = split(strip(parts[2]))
            get!(adj, node, String[])
            append!(adj[node], targets)
        end
    end
    memo = Dict{String,Int64}()
    p1 = count_paths(adj, memo, "svr", "dac") *
         count_paths(adj, memo, "dac", "fft") *
         count_paths(adj, memo, "fft", "out")
    p2 = count_paths(adj, memo, "svr", "fft") *
         count_paths(adj, memo, "fft", "dac") *
         count_paths(adj, memo, "dac", "out")
    println(p1 + p2)
end

main()
