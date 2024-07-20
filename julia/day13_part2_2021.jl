
function main()
    points = Dict{Tuple{Int, Int}, Nothing}()
    folds = Vector{Tuple{Int, Int}}()
    
    open("input.txt") do file
        reading_points = true
        for line in eachline(file)
            if line == ""
                reading_points = false
                continue
            end
            if reading_points
                x, y = parse.(Int, split(line, ","))
                points[(x, y)] = nothing
            else
                parts = split(line, "=")
                val = parse(Int, parts[2])
                if occursin("x", parts[1])
                    push!(folds, (val, 0))
                else
                    push!(folds, (0, val))
                end
            end
        end
    end

    for (i, fold) in enumerate(folds)
        new_points = Dict{Tuple{Int, Int}, Nothing}()
        for (x, y) in keys(points)
            new_point = (x, y)
            if fold[1] != 0 && x > fold[1]
                new_point = (fold[1] - (x - fold[1]), y)
            elseif fold[2] != 0 && y > fold[2]
                new_point = (x, fold[2] - (y - fold[2]))
            end
            new_points[new_point] = nothing
        end
        points = new_points
        if i == 1
            println("Number of dots visible after first fold: ", length(points))
        end
    end

    max_x = maximum(keys(points)) |> x -> x[1]
    max_y = maximum(keys(points)) |> y -> y[2]

    grid = fill(' ', (max_y + 1, max_x + 1))
    for (x, y) in keys(points)
        grid[y + 1, x + 1] = '#'
    end

    for row in eachrow(grid)
        println(String(row))
    end
end

main()
