
numbers = []
open("input.txt") do file
    for line in eachline(file)
        sides = split(line)
        row = [parse(Int, side) for side in sides]
        push!(numbers, row)
    end
end

function isValidTriangle(a, b, c)
    return a + b > c && a + c > b && b + c > a
end

validTriangles = 0
for i in 1:length(numbers[1])
    for j in 1:3:length(numbers)
        if j + 2 <= length(numbers) && isValidTriangle(numbers[j][i], numbers[j+1][i], numbers[j+2][i])
            global validTriangles += 1
        end
    end
end

println(validTriangles)
