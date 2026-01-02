
using Printf

function dist_sq(a, b)
    dx = a[1] - b[1]
    dy = a[2] - b[2]
    dz = a[3] - b[3]
    dx*dx + dy*dy + dz*dz
end

function find(p, x)
    while p[x] != x
        p[x] = p[p[x]]
        x = p[x]
    end
    x
end

function union(p, r, a, b)
    if r[a] < r[b]
        p[a] = b
    elseif r[a] > r[b]
        p[b] = a
    else
        p[b] = a
        r[a] += 1
    end
end

function main()
    lines = filter(!isempty, map(strip, readlines("input.txt")))
    pts = Tuple{Float64,Float64,Float64}[]
    for line in lines
        parts = split(line, ',')
        length(parts) == 3 || continue
        x = tryparse(Float64, parts[1])
        y = tryparse(Float64, parts[2])
        z = tryparse(Float64, parts[3])
        (x === nothing || y === nothing || z === nothing) && continue
        push!(pts, (x, y, z))
    end
    n = length(pts)
    n < 2 && return

    edges = Vector{Tuple{Int,Int,Float64}}()
    for i = 1:n-1, j = i+1:n
        push!(edges, (i, j, dist_sq(pts[i], pts[j])))
    end
    sort!(edges, by = e -> e[3])

    parent = collect(1:n)
    rank = zeros(Int, n)
    comps = n

    for (u, v, _) in edges
        comps == 1 && break
        ru = find(parent, u)
        rv = find(parent, v)
        ru != rv || continue
        union(parent, rank, ru, rv)
        comps -= 1
        if comps == 1
            p1 = pts[u]
            p2 = pts[v]
            @printf "Connected %.0f,%.0f,%.0f and %.0f,%.0f,%.0f\n" p1[1] p1[2] p1[3] p2[1] p2[2] p2[3]
            @printf "Product of X coordinates: %.0f\n" (p1[1] * p2[1])
        end
    end
end

main()
