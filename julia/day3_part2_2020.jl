lines = readlines("input.txt")

slopes = [
    (1, 1),
    (3, 1),
    (5, 1),
    (7, 1),
    (1, 2)
]

product = 1
for slope in slopes
    treeCount = 0
    pos = 1
    for i in 1:slope[2]:length(lines)
        if lines[i][pos] == '#'
            treeCount += 1
        end
        pos = (pos + slope[1] - 1) % length(lines[i]) + 1
    end
    global product *= treeCount
end

println(product)