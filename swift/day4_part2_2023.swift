
import Foundation

func part1(_ lines: [String]) -> Int {
    var totalPoints = 0
    for line in lines {
        let parts = line.components(separatedBy: ": ")
        guard parts.count == 2 else { continue }
        let numberSets = parts[1].components(separatedBy: " | ")
        guard numberSets.count == 2 else { continue }

        let winningNumbers = Set(numberSets[0].split(separator: " ").compactMap { Int($0) })
        let myNumbers = Set(numberSets[1].split(separator: " ").compactMap { Int($0) })

        let matches = winningNumbers.intersection(myNumbers).count
        if matches > 0 {
            totalPoints += 1 << (matches - 1) // Efficiently calculate 2^(matches-1)
        }
    }
    return totalPoints
}

func part2(_ lines: [String]) -> Int {
    var cardCounts = Array(repeating: 1, count: lines.count)

    for (i, line) in lines.enumerated() {
        let parts = line.components(separatedBy: ": ")
        guard parts.count == 2 else { continue }
        let numberSets = parts[1].components(separatedBy: " | ")
        guard numberSets.count == 2 else { continue }
        
        let winningNumbers = Set(numberSets[0].split(separator: " ").compactMap { Int($0) })
        let myNumbers = Set(numberSets[1].split(separator: " ").compactMap { Int($0) })
        
        let matches = winningNumbers.intersection(myNumbers).count
        
        if matches > 0 {
            for j in (i + 1)..<min(i + 1 + matches, lines.count) {
                cardCounts[j] += cardCounts[i]
            }
        }
    }
    return cardCounts.reduce(0, +)
}


func main() {
    do {
        let input = try String(contentsOfFile: "input.txt", encoding: .utf8)
        let lines = input.split(separator: "\n").map { String($0) }

        let part1Result = part1(lines)
        print("Part 1: \(part1Result)")

        let part2Result = part2(lines)
        print("Part 2: \(part2Result)")
    } catch {
        print("Error reading file: \(error)")
    }
}

main()
