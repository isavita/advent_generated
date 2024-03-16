# Read input from file
input = readlines("input.txt")

# Initialize variables
global score = 0
global depth = 0
global inGarbage = false
global cancelNext = false
global garbageCount = 0

# Process stream
for line in input
    for char in line
        if cancelNext
            global cancelNext = false
            continue
        end

        if inGarbage
            if char == '!'
                global cancelNext = true
            elseif char == '>'
                global inGarbage = false
            else
                global garbageCount += 1
            end
        else
            if char == '{'
                global depth += 1
            elseif char == '}'
                global score += depth
                global depth -= 1
            elseif char == '<'
                global inGarbage = true
            end
        end
    end
end

# Print the result
println(garbageCount)