
import Foundation

func readInput(filename: String) -> (orderingRules: [[Int]], updates: [[Int]])? {
    guard let fileURL = URL(string: "file://\(FileManager.default.currentDirectoryPath)/\(filename)"),
          let fileContents = try? String(contentsOf: fileURL) else {
        return nil
    }

    var orderingRules: [[Int]] = []
    var updates: [[Int]] = []
    var isUpdateSection = false

    for line in fileContents.components(separatedBy: .newlines) {
        let trimmedLine = line.trimmingCharacters(in: .whitespaces)
        if trimmedLine.isEmpty {
            isUpdateSection = true
            continue
        }

        if !isUpdateSection {
            let parts = trimmedLine.components(separatedBy: "|")
            guard parts.count == 2,
                  let x = Int(parts[0].trimmingCharacters(in: .whitespaces)),
                  let y = Int(parts[1].trimmingCharacters(in: .whitespaces)) else {
                continue
            }
            orderingRules.append([x, y])
        } else {
            let nums = trimmedLine.components(separatedBy: ",")
            var update: [Int] = []
            for numStr in nums {
                if let num = Int(numStr.trimmingCharacters(in: .whitespaces)) {
                    update.append(num)
                }
            }
            if !update.isEmpty {
                updates.append(update)
            }
        }
    }
    return (orderingRules, updates)
}

func isCorrectlyOrdered(update: [Int], rules: [[Int]]) -> Bool {
    var position: [Int: Int] = [:]
    for (idx, page) in update.enumerated() {
        position[page] = idx
    }

    for rule in rules {
        let x = rule[0], y = rule[1]
        guard let posX = position[x], let posY = position[y] else {
            continue
        }
        if posX >= posY {
            return false
        }
    }
    return true
}

if let (orderingRules, updates) = readInput(filename: "input.txt") {
    var sum = 0
    for update in updates {
        if isCorrectlyOrdered(update: update, rules: orderingRules) {
            sum += update[update.count / 2]
        }
    }
    print(sum)
} else {
    print("Error reading input")
}
