offsets = parse.(Int, readlines("input.txt"))
steps = 0
i = 1

while 1 <= i <= length(offsets)
    global offset = offsets[i]
    global offsets[i] += 1
    global i += offset
    global steps += 1
end

println(steps)