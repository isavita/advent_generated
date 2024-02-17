
data = readlines("input.txt")
offsets = parse.(Int, data)

index = 1
steps = 0

while index >= 1 && index <= length(offsets)
    jump = offsets[index]
    
    if jump >= 3
        offsets[index] -= 1
    else
        offsets[index] += 1
    end
    
    global index += jump  # explicitly declare index as a global variable
    global steps += 1      # explicitly declare steps as a global variable
end

println(steps)
