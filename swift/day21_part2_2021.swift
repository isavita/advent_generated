
import Foundation

func main() {
    let input = try! String(contentsOfFile: "input.txt").trimmingCharacters(in: .whitespacesAndNewlines)
    let result = solve(input)
    print(result)
}

func solve(_ input: String) -> Int64 {
    let positions = parseInput(input)
    var memo = [String: (Int64, Int64)]()
    let (w1, w2) = play(positions: [positions[0], positions[1]], scores: [0, 0], rollsLeftInTurn: 3, isPlayer1sTurn: true, memo: &memo)
    return max(w1, w2)
}

func play(positions: [Int], scores: [Int], rollsLeftInTurn: Int, isPlayer1sTurn: Bool, memo: inout [String: (Int64, Int64)]) -> (Int64, Int64) {
    let key = "\(positions),\(scores),\(rollsLeftInTurn),\(isPlayer1sTurn)"
    if let res = memo[key] {
        return res
    }

    var playerIndex = isPlayer1sTurn ? 0 : 1
    var scoresCopy = scores

    if rollsLeftInTurn == 0 {
        scoresCopy[playerIndex] += positions[playerIndex]
        if scoresCopy[playerIndex] >= 21 {
            return playerIndex == 0 ? (1, 0) : (0, 1)
        }
        playerIndex ^= 1
        return play(positions: positions, scores: scoresCopy, rollsLeftInTurn: 3, isPlayer1sTurn: !isPlayer1sTurn, memo: &memo)
    }

    var wins1: Int64 = 0
    var wins2: Int64 = 0

    for roll in 1...3 {
        var positionsCopy = positions
        positionsCopy[playerIndex] = (positionsCopy[playerIndex] + roll - 1) % 10 + 1
        let (r1, r2) = play(positions: positionsCopy, scores: scoresCopy, rollsLeftInTurn: rollsLeftInTurn - 1, isPlayer1sTurn: isPlayer1sTurn, memo: &memo)
        wins1 += r1
        wins2 += r2
    }

    memo[key] = (wins1, wins2)
    return (wins1, wins2)
}

func parseInput(_ input: String) -> [Int] {
    return input.split(separator: "\n").compactMap {
        let components = $0.split(separator: " ")
        return Int(components[components.count - 1])
    }
}

main()
