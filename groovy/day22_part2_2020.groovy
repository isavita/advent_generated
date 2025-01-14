
def parseInput(String input) {
    def parts = input.split("\n\n")
    def player1Deck = parts[0].split("\n").tail().collect { it.toInteger() }
    def player2Deck = parts[1].split("\n").tail().collect { it.toInteger() }
    return [player1Deck, player2Deck]
}

def combat(List<Integer> player1Deck, List<Integer> player2Deck) {
    while (player1Deck && player2Deck) {
        def card1 = player1Deck.remove(0)
        def card2 = player2Deck.remove(0)
        if (card1 > card2) {
            player1Deck.add(card1)
            player1Deck.add(card2)
        } else {
            player2Deck.add(card2)
            player2Deck.add(card1)
        }
    }
    return player1Deck ? [1, player1Deck] : [2, player2Deck]
}

def recursiveCombat(List<Integer> player1Deck, List<Integer> player2Deck, int gameId = 1) {
    def history = []
    while (player1Deck && player2Deck) {
        def state = [player1Deck.clone(), player2Deck.clone()]
        if (history.contains(state)) {
            return [1, player1Deck]
        }
        history.add(state)

        def card1 = player1Deck.remove(0)
        def card2 = player2Deck.remove(0)

        def winner
        if (player1Deck.size() >= card1 && player2Deck.size() >= card2) {
            def subGamePlayer1Deck = player1Deck.take(card1)
            def subGamePlayer2Deck = player2Deck.take(card2)
            winner = recursiveCombat(subGamePlayer1Deck, subGamePlayer2Deck, gameId + 1)[0]
        } else {
            winner = card1 > card2 ? 1 : 2
        }

        if (winner == 1) {
            player1Deck.add(card1)
            player1Deck.add(card2)
        } else {
            player2Deck.add(card2)
            player2Deck.add(card1)
        }
    }
    return player1Deck ? [1, player1Deck] : [2, player2Deck]
}

def calculateScore(List<Integer> deck) {
    def score = 0
    for (int i = 0; i < deck.size(); i++) {
        score += deck[deck.size() - 1 - i] * (i + 1)
    }
    return score
}

def solvePart1(String input) {
    def (player1Deck, player2Deck) = parseInput(input)
    def (winner, winningDeck) = combat(player1Deck, player2Deck)
    return calculateScore(winningDeck)
}

def solvePart2(String input) {
    def (player1Deck, player2Deck) = parseInput(input)
    def (winner, winningDeck) = recursiveCombat(player1Deck, player2Deck)
    return calculateScore(winningDeck)
}

def input = new File('input.txt').text

println "Part 1: ${solvePart1(input)}"
println "Part 2: ${solvePart2(input)}"
