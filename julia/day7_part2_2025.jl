
#!/usr/bin/env julia
using Base: @inbounds

function add!(d::Dict{Int,BigInt}, k::Int, v::BigInt)
    d[k] = get(d, k, big(0)) + v
end

function main()
    # read non‑empty lines
    lines = filter(!isempty, readlines("input.txt"))
    if isempty(lines)
        println(0)
        return
    end

    h = length(lines)                     # number of rows
    w = length(first(lines))              # number of columns (all rows have same length)

    # locate start point 'S' (zero‑based coordinates)
    sx = -1; sy = -1
    for y in 0:h-1
        row = lines[y+1]
        for x in 0:w-1
            @inbounds if row[x+1] == 'S'
                sx = x; sy = y
                break
            end
        end
        sx != -1 && break
    end
    sx == -1 && error("Start point 'S' not found")

    cur = Dict{Int,BigInt}(sx => big(1))

    for y in sy:h-1                     # process from the start row downwards
        row = lines[y+1]                # convert to 1‑based indexing for the string
        nxt = Dict{Int,BigInt}()

        for (x, cnt) in cur
            if 0 ≤ x < w && row[x+1] == '^'
                add!(nxt, x-1, cnt)
                add!(nxt, x+1, cnt)
            else
                add!(nxt, x, cnt)
            end
        end
        cur = nxt
    end

    total = sum(values(cur))
    println(string(total))
end

main()
