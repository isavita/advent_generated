
def file = new File("input.txt")
def totalScore = 0

file.eachLine { line ->
    def opponent = line[0]
    def yourMove = line[2]

    def score = 0
    if (yourMove == 'X') {
        score = 1
    } else if (yourMove == 'Y') {
        score = 2
    } else if (yourMove == 'Z') {
        score = 3
    }

    if ((opponent == 'A' && yourMove == 'Y') || (opponent == 'B' && yourMove == 'Z') || (opponent == 'C' && yourMove == 'X')) {
        score += 6
    } else if (opponent == 'A' && yourMove == 'X' || opponent == 'B' && yourMove == 'Y' || opponent == 'C' && yourMove == 'Z') {
        score += 3
    }

    totalScore += score
}

println totalScore
