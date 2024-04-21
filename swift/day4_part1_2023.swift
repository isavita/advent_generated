import Foundation

func convertToIntSlice(_ str: String) -> [Int] {
    return str.components(separatedBy: " ").compactMap { Int($0) }
}

func calculatePoints(winningNumbers: [Int], yourNumbers: [Int]) -> Int {
    var points = 0
    for num in yourNumbers {
        if winningNumbers.contains(num) {
            points = points == 0 ? 1 : points * 2
        }
    }
    return points
}

func contains(slice: [Int], val: Int) -> Bool {
    return slice.contains(val)
}

do {
    let fileURL = URL(fileURLWithPath: "input.txt")
    let content = try String(contentsOf: fileURL, encoding: .utf8)
    let lines = content.components(separatedBy: "\n")
    var totalPoints = 0
    for line in lines {
        let parts = line.components(separatedBy: " | ")
        let winningNumbers = convertToIntSlice(parts[0])
        let yourNumbers = convertToIntSlice(parts[1])
        totalPoints += calculatePoints(winningNumbers: winningNumbers, yourNumbers: yourNumbers)
    }
    print(totalPoints)
} catch {
    print("Error reading file:", error)
}