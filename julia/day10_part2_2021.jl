using Dates

function checkAndCompleteLine(line)
    pairings = Dict(')'=>'(', ']'=>'[', '}'=>'{', '>'=>'<')
    scoreValues = Dict(')'=>1, ']'=>2, '}'=>3, '>'=>4)
    opening = "([{<"
    closing = ")]}>"
    stack = []

    for char in line
        if char in opening
            push!(stack, char)
        elseif char in closing
            if isempty(stack) || stack[end] != pairings[char]
                return 0, false # corrupted line
            end
            pop!(stack) # pop from stack
        end
    end

    if isempty(stack)
        return 0, false # line is not incomplete
    end

    # Calculate score for incomplete line
    score = 0
    for i in length(stack):-1:1
        score *= 5
        score += scoreValues[getClosingChar(stack[i])]
    end
    return score, true
end

function getClosingChar(openingChar)
    if openingChar == '('
        return ')'
    elseif openingChar == '['
        return ']'
    elseif openingChar == '{'
        return '}'
    elseif openingChar == '<'
        return '>'
    else
        return ' '
    end
end

# Read input from file
input = readlines("input.txt")

scores = []
for line in input
    score, incomplete = checkAndCompleteLine(line)
    if incomplete
        push!(scores, score)
    end
end

sort!(scores)
middleScore = scores[ceil(Int, length(scores)/2)]
println(middleScore)