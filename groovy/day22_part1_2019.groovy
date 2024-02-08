
def deck = new int[10007]
(0..10006).each { deck[it] = it }

def dealIntoNewStack(deck) {
    (0..4998).each { i ->
        def temp = deck[i]
        deck[i] = deck[10006 - i]
        deck[10006 - i] = temp
    }
    return deck
}

def cutN(deck, n) {
    if (n >= 0) {
        return deck[n..-1] + deck[0..n-1]
    } else {
        return deck[n+10007..-1] + deck[0..n+10006]
    }
}

def dealWithIncrement(deck, n) {
    def newDeck = new int[10007]
    (0..10006).each { i ->
        newDeck[(i * n) % 10007] = deck[i]
    }
    return newDeck
}

new File("input.txt").eachLine { line ->
    if (line == "deal into new stack") {
        deck = dealIntoNewStack(deck)
    } else if (line.startsWith("cut")) {
        def n = line.tokenize(" ")[1] as int
        deck = cutN(deck, n)
    } else if (line.startsWith("deal with increment")) {
        def n = line.tokenize(" ")[-1] as int
        deck = dealWithIncrement(deck, n)
    }
}

println deck.findIndexOf { it == 2019 }
