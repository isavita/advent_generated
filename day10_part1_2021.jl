
using DelimitedFiles

function checkLine(line)
    pairings = Dict(')' => '(', ']' => '[', '}' => '{', '>' => '<')
    scores = Dict(')' => 3, ']' => 57, '}' => 1197, '>' => 25137)
    stack = Char[]

    for char in line
        if char in keys(pairings)
            if isempty(stack) || stack[end] != pairings[char]
                return scores[char], true
            end
            pop!(stack)
        elseif char in values(pairings)
            push!(stack, char)
        end
    end
    return 0, false
end

totalScore = 0
for line in eachline("input.txt")
    score, corrupted = checkLine(line)
    if corrupted
        global totalScore += score
    end
end

println(totalScore)
