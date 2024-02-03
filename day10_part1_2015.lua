
function readInput(filename)
    local file = io.open(filename, "r")
    local sequence = file:read("*line")
    file:close()
    return sequence
end

function lookAndSay(sequence, iterations)
    for i = 1, iterations do
        sequence = nextSequence(sequence)
    end
    return sequence
end

function nextSequence(sequence)
    local result = ""
    local i = 1
    while i <= #sequence do
        local count, digit = 1, string.sub(sequence, i, i)
        local j = i + 1
        while j <= #sequence and string.sub(sequence, j, j) == digit do
            count = count + 1
            j = j + 1
        end
        result = result .. count .. digit
        i = j
    end
    return result
end

local initialSequence = readInput("input.txt")
local result = lookAndSay(initialSequence, 40)
print(#result)
