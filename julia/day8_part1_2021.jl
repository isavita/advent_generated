# Read input from "input.txt"
input = readlines("input.txt")

global count = 0
for line in input
    parts = split(line, " | ")
    output = parts[2]
    for digit in split(output, " ")
        if length(digit) in [2, 4, 3, 7]
            global count += 1
        end
    end
end

println(count)