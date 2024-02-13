
import Foundation

func parseInput(input: [String]) -> [[Int]] {
    var histories: [[Int]] = []
    for line in input {
        let numbers = line.split(separator: " ").compactMap { Int($0) }
        histories.append(numbers)
    }
    return histories
}

func allZeros(nums: [Int]) -> Bool {
    for num in nums {
        if num != 0 {
            return false
        }
    }
    return true
}

func calculateExtrapolation(history: [Int]) -> [Int] {
    var extrapolations: [Int] = []
    for i in 1..<history.count {
        let extrapolation = history[i] - history[i-1]
        extrapolations.append(extrapolation)
    }
    return extrapolations
}

func calculateExtrapolations(history: [Int]) -> [[Int]] {
    var extrapolationsSeries: [[Int]] = []
    extrapolationsSeries.append(history)

    for i in 1..<history.count {
        let previousExtrapolations = extrapolationsSeries[i-1]
        if allZeros(nums: previousExtrapolations) {
            return extrapolationsSeries
        }

        let extrapolations = calculateExtrapolation(history: previousExtrapolations)
        extrapolationsSeries.append(extrapolations)
    }

    return extrapolationsSeries
}

func solve(input: [String]) -> Int {
    let histories = parseInput(input: input)
    var res = 0

    for history in histories {
        let extrapolationsSeries = calculateExtrapolations(history: history)

        var futurePrediction = 0
        for i in (0..<extrapolationsSeries.count).reversed() {
            futurePrediction = extrapolationsSeries[i][extrapolationsSeries[i].count-1] + futurePrediction
        }

        res += futurePrediction
    }

    return res
}

func readFile(fileName: String) -> [String] {
    do {
        let fileContent = try String(contentsOfFile: fileName)
        return fileContent.components(separatedBy: "\n")
    } catch {
        fatalError("Error reading file: \(error)")
    }
}

let input = readFile(fileName: "input.txt")
print(solve(input: input))
