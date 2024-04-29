def scoreboard = [3, 7]
def elf1 = 0
def elf2 = 1
def input = new File('input.txt').text.trim()
def inputSequence = input.collect { it as int }

while (true) {
    def newScore = scoreboard[elf1] + scoreboard[elf2]
    if (newScore >= 10) {
        scoreboard << newScore.intdiv(10)
        if (checkSequence(scoreboard, inputSequence)) {
            break
        }
    }
    scoreboard << newScore % 10
    if (checkSequence(scoreboard, inputSequence)) {
        break
    }

    elf1 = (elf1 + scoreboard[elf1] + 1) % scoreboard.size()
    elf2 = (elf2 + scoreboard[elf2] + 1) % scoreboard.size()
}

println scoreboard.size() - inputSequence.size()

def checkSequence(scoreboard, sequence) {
    if (scoreboard.size() < sequence.size()) {
        return false
    }
    def start = scoreboard.size() - sequence.size()
    for (int i = 0; i < sequence.size(); i++) {
        if (scoreboard[start + i] != sequence[i]) {
            return false
        }
    }
    return true
}