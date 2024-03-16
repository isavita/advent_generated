open("input.txt", "r") do file
    line = readline(file)
    steps = split(line, ",")
    sum = 0
    for step in steps
        hash = 0
        for char in step
            if char != ','
                hash = (hash + Int(char)) * 17
                hash = hash % 256
            end
        end
        sum += hash
    end
    println(sum)
end