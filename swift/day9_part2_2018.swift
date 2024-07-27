
import Foundation

class Marble {
    var value: Int
    var prev: Marble?
    var next: Marble?

    init(value: Int) {
        self.value = value
    }
}

func main() {
    let (players, lastMarble) = readInput("input.txt")
    let result = playMarbleGame(players: players, lastMarble: lastMarble * 100)
    print(result)
}

func readInput(_ filename: String) -> (Int, Int) {
    let content = try! String(contentsOfFile: filename)
    let parts = content.split(separator: " ")
    let players = Int(parts[0])!
    let lastMarble = Int(parts[6])!
    return (players, lastMarble)
}

func playMarbleGame(players: Int, lastMarble: Int) -> Int {
    var scores = [Int](repeating: 0, count: players)
    let current = Marble(value: 0)
    current.next = current
    current.prev = current

    var currentMarble = current

    for marble in 1...lastMarble {
        if marble % 23 == 0 {
            let player = marble % players
            for _ in 0..<7 {
                currentMarble = currentMarble.prev ?? currentMarble
            }
            scores[player] += marble + (currentMarble.value)
            currentMarble.prev?.next = currentMarble.next
            currentMarble.next?.prev = currentMarble.prev
            currentMarble = currentMarble.next ?? currentMarble
        } else {
            currentMarble = currentMarble.next ?? currentMarble
            let newMarble = Marble(value: marble)
            newMarble.prev = currentMarble
            newMarble.next = currentMarble.next
            currentMarble.next?.prev = newMarble
            currentMarble.next = newMarble
            currentMarble = newMarble
        }
    }

    return scores.max() ?? 0
}

main()
