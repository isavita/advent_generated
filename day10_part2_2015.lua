
local file = io.open("input.txt", "r")
local initialSequence = file:read("*line")
file:close()

function lookAndSay(sequence, iterations)
    for i = 1, iterations do
        sequence = nextSequence(sequence)
    end
    return sequence
end

function nextSequence(sequence)
    local result = {}
    local i = 1
    while i <= #sequence do
        local count, digit = 1, sequence:sub(i, i)
        local j = i + 1
        while j <= #sequence and sequence:sub(j, j) == digit do
            count = count + 1
            j = j + 1
        end
        table.insert(result, count .. digit)
        i = i + count
    end
    return table.concat(result)
end

local result = lookAndSay(initialSequence, 50)
print(#result)
