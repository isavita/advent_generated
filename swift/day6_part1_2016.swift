
import Foundation

if let input = try? String(contentsOfFile: "input.txt") {
    let lines = input.components(separatedBy: .newlines)
    let messageLength = lines.first?.count ?? 0
    var result = ""

    for i in 0..<messageLength {
        var charCount: [Character: Int] = [:]

        for line in lines {
            let index = line.index(line.startIndex, offsetBy: i)
            let char = line[index]

            if let count = charCount[char] {
                charCount[char] = count + 1
            } else {
                charCount[char] = 1
            }
        }

        if let maxChar = charCount.max(by: { $0.value < $1.value })?.key {
            result.append(maxChar)
        }
    }

    print(result)
}
