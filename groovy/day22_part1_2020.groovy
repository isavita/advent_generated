def input = new File("input.txt").readLines()

def player1 = input.drop(1).takeWhile { it != "" }.collect { it.toInteger() }
def player2 = input.dropWhile { it != "Player 2:" }.drop(1).collect { it.toInteger() }

while (player1.size() > 0 && player2.size() > 0) {
    def card1 = player1.remove(0)
    def card2 = player2.remove(0)
    if (card1 > card2) {
        player1.add(card1)
        player1.add(card2)
    } else {
        player2.add(card2)
        player2.add(card1)
    }
}

def winner = player1.size() > 0 ? player1 : player2

def score = 0
for (int i = 0; i < winner.size(); i++) {
    score += winner[i] * (winner.size() - i)
}

println score