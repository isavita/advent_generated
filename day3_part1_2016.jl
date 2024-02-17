
validTriangles = 0

function isValidTriangle(a, b, c)
    return a + b > c && a + c > b && b + c > a
end

open("input.txt") do file
    for line in eachline(file)
        sides = split(line)
        if length(sides) != 3
            println("Invalid input format")
            continue
        end

        a, b, c = parse.(Int, sides)

        if isValidTriangle(a, b, c)
            global validTriangles += 1
        end
    end
end

println(validTriangles)
