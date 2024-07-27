
import Foundation

func readExpenses(from file: String) -> [Int] {
    guard let contents = try? String(contentsOfFile: file) else { return [] }
    return contents.split(separator: "\n").compactMap { Int($0) }
}

func findProductOfThreeExpenses(expenses: [Int], target: Int) -> Int? {
    let expenseSet = Set(expenses)
    for i in 0..<expenses.count {
        for j in i + 1..<expenses.count {
            let needed = target - expenses[i] - expenses[j]
            if expenseSet.contains(needed) {
                return expenses[i] * expenses[j] * needed
            }
        }
    }
    return nil
}

let expenses = readExpenses(from: "input.txt")
if let product = findProductOfThreeExpenses(expenses: expenses, target: 2020) {
    print(product)
}
