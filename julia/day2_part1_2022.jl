
totalScore = 0

open("input.txt") do file
    for line in eachline(file)
        opponent = line[1]
        yourMove = line[3]

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

        global totalScore += score
    end
end

println(totalScore)
