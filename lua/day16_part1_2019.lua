-- Function to read input from file
local function read_input(filename)
    local file = io.open(filename, "r")
    local input = file:read("*all")
    file:close()
    return input
end

-- Function to generate the pattern for a given position
local function generate_pattern(position, length)
    local base_pattern = {0, 1, 0, -1}
    local pattern = {}
    local index = 1

    while #pattern <= length do
        for _ = 1, position do
            table.insert(pattern, base_pattern[index])
            if #pattern > length then break end
        end
        index = index % #base_pattern + 1
    end

    table.remove(pattern, 1) -- Remove the first element as per the problem statement
    return pattern
end

-- Function to perform one phase of FFT
local function fft_phase(input)
    local output = {}
    for i = 1, #input do
        local pattern = generate_pattern(i, #input + 1)
        local sum = 0
        for j = 1, #input do
            sum = sum + input[j] * pattern[j]
        end
        table.insert(output, math.abs(sum) % 10)
    end
    return output
end

-- Main function to perform FFT for a number of phases
local function perform_fft(input, phases)
    local signal = {}
    for digit in input:gmatch("%d") do
        table.insert(signal, tonumber(digit))
    end

    for _ = 1, phases do
        signal = fft_phase(signal)
    end

    return table.concat(signal, "", 1, 8) -- Return the first eight digits
end

-- Read input from file
local input = read_input("input.txt")

-- Perform 100 phases of FFT
local result = perform_fft(input, 100)

-- Print the first eight digits of the final output list
print(result)