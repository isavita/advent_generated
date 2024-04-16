function applyFFT(input)
    basePattern = [0, 1, 0, -1]
    output = zeros(Int, length(input))
    for i in 1:length(input)
        sum = 0
        for (j, val) in enumerate(input)
            patternValue = basePattern[mod(div(j, i), length(basePattern)) + 1]
            sum += val * patternValue
        end
        output[i] = abs(sum % 10)
    end
    return output
end

# Read input from file
input = readline("input.txt")

# Convert input string to array of ints
global digits = [parse(Int, c) for c in input]

# Apply FFT algorithm for 100 phases
for phase in 1:100
    global digits = applyFFT(digits)
end

# Output the first eight digits
println(join(digits[1:8]))