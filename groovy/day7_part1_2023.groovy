
def file = new File("input.txt")
def lines = file.text.split("\n")
def hands = []

def re = /[\dAKQJT]+/
def bidRe = / [\d]+/

lines.each { line ->
    if (line.size() == 0) {
        return
    }

    def cards = (line =~ re)[0]
    def bid = (line =~ bidRe)[0][1..-1] as Integer

    hands.add([cards: cards, bid: bid])
}

def matches = [[], [], [], [], [], [], []]

hands.each { hand ->
    def count = [:]

    hand.cards.each { card ->
        count[card] = count.getOrDefault(card, 0) + 1
    }

    def value = 1
    count.each { k, v ->
        value *= v
    }

    switch (value) {
        case 1:
            matches[6] << hand
            break
        case 2:
            matches[5] << hand
            break
        case 3:
            matches[3] << hand
            break
        case 4:
            if (count.size() == 2) {
                matches[1] << hand
            } else {
                matches[4] << hand
            }
            break
        case 5:
            matches[0] << hand
            break
        case 6:
            matches[2] << hand
            break
        default:
            println("oh no")
    }
}

def convertAndOrderMatches = {
    def convertedMatches = []

    matches.each { category ->
        def temp = []

        category.each { hand ->
            def cards = hand.cards.replaceAll("A", "E").replaceAll("T", "A").replaceAll("J", "B").replaceAll("Q", "C").replaceAll("K", "D")
            def num = Integer.parseInt(cards, 16)

            temp << [hand: hand, rank: num]
        }

        temp.sort { a, b -> b.rank <=> a.rank }

        convertedMatches.addAll(temp)
    }

    return convertedMatches
}

def convertedMatches = convertAndOrderMatches()

def total = 0
convertedMatches.eachWithIndex { rankedHand, index ->
    total += rankedHand.hand.bid * (convertedMatches.size() - index)
}

println(total)
