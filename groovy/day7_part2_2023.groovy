
def valueDict = ['J': 1, '2': 2, '3': 3, '4': 4, '5': 5, '6': 6, '7': 7, '8': 8, '9': 9, 'T': 10, 'Q': 11, 'K': 12, 'A': 13]

class Hand {
    String cards
    int bid

    Hand(String cards, int bid) {
        this.cards = cards
        this.bid = bid
    }
}

def solve() {
    def lines = new File("input.txt").readLines()
    def hands = []

    lines.each { line ->
        if (line) {
            def parts = line.split(" ")
            hands << new Hand(parts[0], parts[1].toInteger())
        }
    }

    def matches = [[], [], [], [], [], [], []]

    hands.each { hand ->
        def count = [:]
        hand.cards.each { card ->
            count[card] = count.getOrDefault(card, 0) + 1
        }

        if (count['J']) {
            def jCount = count['J']
            count.remove('J')
            if (!count.isEmpty()) {
                def highCard = count.max { it.value }?.key
                count[highCard] = count.getOrDefault(highCard, 0) + jCount
            } else {
                count['A'] = jCount
            }
        }


        def value = 1
        count.values().each { value *= it }

        def typeIndex
        if (value == 1) {
            typeIndex = 6
        } else if (value == 2) {
            typeIndex = 5
        } else if (value == 3) {
            typeIndex = 3
        } else if (value == 4) {
            typeIndex = count.size() == 2 ? 1 : 4
        } else if (value == 5) {
            typeIndex = 0
        } else if (value == 6) {
            typeIndex = 2
        } else {
            println "oh no"
            return
        }
        matches[typeIndex] << hand
    }

    def convertedMatches = []
    matches.each { handList ->
        def temp = handList.collect { hand ->
            def y = hand.cards.replace("A", "E").replace("T", "A").replace("J", "1").replace("Q", "C").replace("K", "D")
            def val = Integer.parseInt(y, 16)
            [val, hand.bid]
        }.sort { a, b -> b[0] <=> a[0] }
        convertedMatches.addAll(temp)
    }

    def total = 0
    for (int x = 0; x < convertedMatches.size(); x++) {
        total += convertedMatches[x][1] * (convertedMatches.size() - x)
    }

    println total
}

solve()
