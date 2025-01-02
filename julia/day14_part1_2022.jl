
function solve()
    grid = Set{Tuple{Int, Int}}()
    for line in eachline("input.txt")
        sp = split(line, " -> ")
        pts = map(x -> parse.(Int, split(x, ",")), sp)
        for i in 1:length(pts)-1
            if pts[i][1] == pts[i+1][1]
                for y in min(pts[i][2], pts[i+1][2]):max(pts[i][2], pts[i+1][2])
                    push!(grid, (pts[i][1], y))
                end
            else
                for x in min(pts[i][1], pts[i+1][1]):max(pts[i][1], pts[i+1][1])
                    push!(grid, (x, pts[i][2]))
                end
            end
        end
    end

    floor = maximum(y for (x, y) in grid) + 1
    sands, firstFloorTouch = 0, 0
    while (500, 0) ∉ grid
        sand = (500, 0)
        settled = false
        while !settled
            if sand[2] == floor
                if firstFloorTouch == 0
                    firstFloorTouch = sands
                end
                push!(grid, sand)
                break
            end
            
            next_sand = nothing
            for n in [(sand[1], sand[2] + 1), (sand[1] - 1, sand[2] + 1), (sand[1] + 1, sand[2] + 1)]
                if n ∉ grid
                    next_sand = n
                    break
                end
            end
            
            if next_sand === nothing
                push!(grid, sand)
                settled = true
            else
                sand = next_sand
            end
        end
        sands += 1
    end
    println(firstFloorTouch)
end

solve()
