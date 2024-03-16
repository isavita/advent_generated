open("input.txt", "r") do file
    steps = parse(Int, readline(file))
    currentPos = 0
    valueAfterZero = 0

    for i in 1:50000000
        currentPos = (currentPos + steps) % i
        if currentPos == 0
            valueAfterZero = i
        end
        currentPos += 1
    end

    println(valueAfterZero)
end