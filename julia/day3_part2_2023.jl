using Images

neighbors8 = [(0, 1), (0, -1), (1, 0), (-1, 0), (-1, -1), (-1, 1), (1, -1), (1, 1)]

mutable struct Part
    xmin::Int
    xmax::Int
    y::Int
    n::Int
end

function valid(p::Part, grid)
    for x in p.xmin:p.xmax
        for n in neighbors8
            c = get(grid, (x + n[1], p.y + n[2]), '.')
            if c != '.' && (c < '0' || c > '9')
                return true
            end
        end
    end
    return false
end

function main()
    buf = read("input.txt", String)
    input = strip(buf)

    grid = Dict{Tuple{Int,Int},Char}()
    parts = Part[]
    curr = nothing
    for (y, line) in enumerate(split(input, "\n"))
        if curr !== nothing
            push!(parts, curr)
            curr = nothing
        end
        for (x, c) in enumerate(line)
            grid[(x-1, y-1)] = c
            if '0' <= c <= '9'
                if curr === nothing
                    curr = Part(x-1, x-1, y-1, parse(Int, c))
                else
                    curr.n = curr.n * 10 + parse(Int, c)
                    curr.xmax = x-1
                end
            elseif curr !== nothing
                push!(parts, curr)
                curr = nothing
            end
        end
    end

    parts_grid = Dict{Tuple{Int,Int},Int}()
    for (i, p) in enumerate(parts)
        for x in p.xmin:p.xmax
            parts_grid[(x, p.y)] = i
        end
    end

    sum = 0
    for (p, c) in grid
        if c == '*'
            neighbor_parts = Set{Int}()
            for n in neighbors8
                if haskey(parts_grid, (p[1] + n[1], p[2] + n[2]))
                    push!(neighbor_parts, parts_grid[(p[1] + n[1], p[2] + n[2])])
                end
            end
            if length(neighbor_parts) == 2
                prod = 1
                for i in neighbor_parts
                    prod *= parts[i].n
                end
                sum += prod
            end
        end
    end
    println(sum)
end

main()