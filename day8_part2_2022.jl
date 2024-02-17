using DelimitedFiles

const Neighbors4 = [(0, 1), (0, -1), (1, 0), (-1, 0)]

function main()
    grid = Dict{Tuple{Int, Int}, Int}()
    y = 0
    for line in eachline("input.txt")
        x = 0
        for c in line
            grid[(x, y)] = parse(Int, c)
            x += 1
        end
        y += 1
    end

    maxScore = 0
    for p in keys(grid)
        score = 1
        for n in Neighbors4
            next, view = p, 0
            while true
                next = (next[1] + n[1], next[2] + n[2])
                if haskey(grid, next)
                    view += 1
                    if grid[next] >= grid[p]
                        score *= view
                        break
                    end
                else
                    score *= view
                    break
                end
            end
        end

        if score > maxScore
            maxScore = score
        end
    end
    println(maxScore)
end

main()