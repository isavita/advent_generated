
import Foundation

func main() {
    guard let url = URL(string: "file://\(FileManager.default.currentDirectoryPath)/input.txt") else {
        fatalError("Invalid file path")
    }

    do {
        let content = try String(contentsOf: url)
        let lines = content.components(separatedBy: .newlines)
        var left = [Int]()
        var right = [Int]()

        for (index, line) in lines.enumerated() {
            let fields = line.split(separator: " ")
            guard fields.count == 2 else {
                fatalError("Invalid input format at line \(index + 1): expected 2 numbers, got \(fields.count)")
            }
            guard let leftNum = Int(fields[0]), let rightNum = Int(fields[1]) else {
                fatalError("Invalid number at line \(index + 1)")
            }
            left.append(leftNum)
            right.append(rightNum)
        }

        guard left.count == right.count else {
            fatalError("Lists have different lengths")
        }

        let totalDistance = zip(left.sorted(), right.sorted()).reduce(0) { $0 + abs($1.0 - $1.1) }
        print(totalDistance)

    } catch {
        fatalError("Failed to read file: \(error)")
    }
}

main()
