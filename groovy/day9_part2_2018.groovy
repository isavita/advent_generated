
class Marble {
    int value
    Marble prev
    Marble next
}

def solve() {
    def input = new File("input.txt").text.split()
    def players = input[0].toInteger()
    def lastMarble = input[6].toInteger() * 100

    def scores = new long[players]
    def current = new Marble(value: 0)
    current.next = current
    current.prev = current

    for (int marble = 1; marble <= lastMarble; marble++) {
        if (marble % 23 == 0) {
            def player = marble % players
            for (int i = 0; i < 7; i++) {
                current = current.prev
            }
            scores[player] += marble + current.value
            current.prev.next = current.next
            current.next.prev = current.prev
            current = current.next
        } else {
            current = current.next
            def newMarble = new Marble(value: marble, prev: current, next: current.next)
            current.next.prev = newMarble
            current.next = newMarble
            current = newMarble
        }
    }

    println scores.max()
}

solve()
