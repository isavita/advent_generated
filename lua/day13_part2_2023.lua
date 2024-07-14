-- Function to read the input file
function readInput()
    local patterns = {}
    local currentPattern = {}
    for line in io.lines("input.txt") do
        if line == "" then
            table.insert(patterns, currentPattern)
            currentPattern = {}
        else
            table.insert(currentPattern, line)
        end
    end
    table.insert(patterns, currentPattern)
    return patterns
end

-- Function to find reflection with one smudge fixed
function findReflectionWithSmudge(pattern)
    local rows, cols = #pattern, #pattern[1]

    -- Check horizontal reflections
    for i = 1, rows - 1 do
        local differences = 0
        for j = 1, math.min(i, rows - i) do
            for k = 1, cols do
                if pattern[i-j+1]:sub(k,k) ~= pattern[i+j]:sub(k,k) then
                    differences = differences + 1
                end
            end
        end
        if differences == 1 then
            return i * 100
        end
    end

    -- Check vertical reflections
    for i = 1, cols - 1 do
        local differences = 0
        for j = 1, math.min(i, cols - i) do
            for k = 1, rows do
                if pattern[k]:sub(i-j+1,i-j+1) ~= pattern[k]:sub(i+j,i+j) then
                    differences = differences + 1
                end
            end
        end
        if differences == 1 then
            return i
        end
    end

    return 0
end

-- Main function
function main()
    local patterns = readInput()
    local total = 0

    for _, pattern in ipairs(patterns) do
        total = total + findReflectionWithSmudge(pattern)
    end

    print(total)
end

main()
