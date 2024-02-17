
totalScore = 0

for line in eachline("input.txt")
    opponent = line[1]
    roundEnd = line[3]

    if roundEnd == 'X'
        if opponent == 'A'
            yourMove = 'Z'
        elseif opponent == 'B'
            yourMove = 'X'
        else
            yourMove = 'Y'
        end
    elseif roundEnd == 'Y'
        if opponent == 'A'
            yourMove = 'X'
        elseif opponent == 'B'
            yourMove = 'Y'
        else
            yourMove = 'Z'
        end
    else
        if opponent == 'A'
            yourMove = 'Y'
        elseif opponent == 'B'
            yourMove = 'Z'
        else
            yourMove = 'X'
        end
    end

    score = 0
    if yourMove == 'X'
        score = 1
    elseif yourMove == 'Y'
        score = 2
    elseif yourMove == 'Z'
        score = 3
    end

    if (opponent == 'A' && yourMove == 'Y') || (opponent == 'B' && yourMove == 'Z') || (opponent == 'C' && yourMove == 'X')
        score += 6
    elseif (opponent == 'A' && yourMove == 'X') || (opponent == 'B' && yourMove == 'Y') || (opponent == 'C' && yourMove == 'Z')
        score += 3
    end

    global totalScore
    totalScore += score
end

println(totalScore)
