
import Foundation

func evaluateExpression(testValue: Int, numbers: [Int]) -> String? {
    let operators = ["+", "*", "||"]
    let numOperators = numbers.count - 1

    func generateCombinations(index: Int, currentCombination: [String], result: inout [String]) {
        if index == numOperators {
            result.append(currentCombination.joined(separator: " "))
            return
        }

        for op in operators {
            generateCombinations(index: index + 1, currentCombination: currentCombination + [op], result: &result)
        }
    }

    var allCombinations: [String] = []
    generateCombinations(index: 0, currentCombination: [], result: &allCombinations)

    for opsString in allCombinations {
        let ops = opsString.components(separatedBy: " ")
        var result = numbers[0]

        for i in 0..<numOperators {
            switch ops[i] {
            case "+":
                result += numbers[i + 1]
            case "*":
                result *= numbers[i + 1]
            case "||":
                let combinedString = String(result) + String(numbers[i + 1])
                if let combinedInt = Int(combinedString) {
                    result = combinedInt
                } else {
                    result = -1
                    break
                }
            default:
                break
            }
        }

        if result == testValue {
            var expression = String(numbers[0])
            for i in 0..<numOperators {
                expression += " \(ops[i]) \(numbers[i+1])"
            }
            return expression
        }
    }
    return nil
}

func calculateSumOfTestValues(testValues: [(Int, [Int])]) -> Int {
    var sumOfTestValues = 0
    for testValue in testValues {
        if evaluateExpression(testValue: testValue.0, numbers: testValue.1) != nil {
            sumOfTestValues += testValue.0
        }
    }
    return sumOfTestValues
}

do {
    let fileURL = URL(fileURLWithPath: "input.txt")
    let input = try String(contentsOf: fileURL, encoding: .utf8).trimmingCharacters(in: .whitespacesAndNewlines)

    let lines = input.components(separatedBy: "\n")
    var testValues: [(Int, [Int])] = []

    for line in lines {
        let parts = line.components(separatedBy: ": ")
        if parts.count == 2, let testValue = Int(parts[0]) {
            let numbers = parts[1].components(separatedBy: " ").compactMap { Int($0) }
            testValues.append((testValue, numbers))
        }
    }

    let sum = calculateSumOfTestValues(testValues: testValues)
    print(sum)

} catch {
    print("Error reading or processing input file: \(error)")
}
