
function main()
    data = readlines("input.txt")
    points = Tuple{Int,Int,Int}[]
    for line in data
        line = strip(line)
        isempty(line) && continue
        x, y, z = parse.(Int, split(line, ','))
        push!(points, (x, y, z))
    end
    n = length(points)
    n < 2 && (println("Not enough points to form circuits."); return)

    m = n * (n - 1) ÷ 2
    edges = Vector{Tuple{Int,Int,Int}}(undef, m)
    idx = 1
    for i = 1:n-1
        p1 = points[i]
        for j = i+1:n
            p2 = points[j]
            dx = p1[1] - p2[1]
            dy = p1[2] - p2[2]
            dz = p1[3] - p2[3]
            d = dx * dx + dy * dy + dz * dz
            edges[idx] = (d, i, j)
            idx += 1
        end
    end
    sort!(edges, by = e -> e[1])

    parent = collect(1:n)
    sz = ones(Int, n)

    find(i) = begin
        while parent[i] != i
            parent[i] = parent[parent[i]]
            i = parent[i]
        end
        i
    end

    union(i, j) = begin
        ri = find(i)
        rj = find(j)
        ri == rj && return
        if sz[ri] < sz[rj]
            ri, rj = rj, ri
        end
        parent[rj] = ri
        sz[ri] += sz[rj]
    end

    limit = min(1000, length(edges))
    for k = 1:limit
        e = edges[k]
        union(e[2], e[3])
    end

    comps = Int[]
    for i = 1:n
        parent[i] == i && push!(comps, sz[i])
    end
    sort!(comps, rev = true)

    result = BigInt(1)
    for v in comps[1:min(3, length(comps))]
        result *= BigInt(v)
    end
    println("Product of three largest circuit sizes: $(result)")
end

main()
