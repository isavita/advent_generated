struct Point
    x::Int
    y::Int
end

function main()
    points = Dict{Point,Bool}()
    folds = String[]

    open("input.txt", "r") do file
        readingPoints = true
        for line in eachline(file)
            if isempty(line)
                readingPoints = false
                continue
            end
            if readingPoints
                x, y = map(x -> parse(Int, x), split(line, ","))
                points[Point(x, y)] = true
            else
                push!(folds, line)
            end
        end
    end

    fold = split(folds[1], " ")[3] # "fold along x=5" -> "x=5"
    axis, value = split(fold, "=")
    value = parse(Int, value)

    newPoints = Dict{Point,Bool}()
    if axis == "x"
        for (point, _) in points
            newX = point.x > value ? 2 * value - point.x : point.x
            newPoints[Point(newX, point.y)] = true
        end
    else
        for (point, _) in points
            newY = point.y > value ? 2 * value - point.y : point.y
            newPoints[Point(point.x, newY)] = true
        end
    end

    println(length(newPoints))
end

main()