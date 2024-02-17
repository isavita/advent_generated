
using Printf

function main()
    grid = Dict{Tuple{Int, Int}, Int}()
    visible = Dict{Tuple{Int, Int}, Nothing}()
    y = 0
    for line in eachline("input.txt")
        x = 0
        for b in line
            grid[(x, y)] = parse(Int, b) - parse(Int, '0')
            x += 1
        end
        y += 1
    end

    for p in keys(grid)
        for n in [(0, 1), (0, -1), (1, 0), (-1, 0)]
            next = p
            while true
                next = (next[1] + n[1], next[2] + n[2])
                if haskey(grid, next)
                    if grid[next] >= grid[p]
                        break
                    end
                else
                    visible[p] = nothing
                    break
                end
            end
        end
    end
    println(length(visible))
end

main()
