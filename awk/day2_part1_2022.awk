
{
    opponent = substr($0, 1, 1)
    yourMove = substr($0, 3, 1)

    score = 0
    if (yourMove == "X") {
        score = 1
    } else if (yourMove == "Y") {
        score = 2
    } else if (yourMove == "Z") {
        score = 3
    }

    if ((opponent == "A" && yourMove == "Y") || (opponent == "B" && yourMove == "Z") || (opponent == "C" && yourMove == "X")) {
        score += 6
    } else if ((opponent == "A" && yourMove == "X") || (opponent == "B" && yourMove == "Y") || (opponent == "C" && yourMove == "Z")) {
        score += 3
    }

    totalScore += score
}

END {
    print totalScore
}
