
data = readlines("input.txt")[1]
banks = parse.(Int, split(data))

seen = Dict()
cycles = 0

while true
    state = string(banks)
    if haskey(seen, state)
        break
    end
    seen[state] = true

    maxIndex = argmax(banks)
    blocks = banks[maxIndex]
    banks[maxIndex] = 0
    for i in 1:blocks
        banks[(maxIndex + i - 1) % length(banks) + 1] += 1
    end

    global cycles += 1
end

println("It takes ", cycles, " redistribution cycles to reach a repeated configuration.")
