import Foundation

let fileURL = URL(fileURLWithPath: "input.txt")
let contents = try String(contentsOf: fileURL, encoding: .utf8)
let lines = contents.components(separatedBy: "\n")

var validCountPart1 = 0
var validCountPart2 = 0

for line in lines {
    let components = line.components(separatedBy: " ")
    let range = components[0].components(separatedBy: "-")
    let char = components[1].first!
    let password = components[2]

    let minCount = Int(range[0])!
    let maxCount = Int(range[1])!

    let charCount = password.filter { $0 == char }.count

    if charCount >= minCount && charCount <= maxCount {
        validCountPart1 += 1
    }

    let firstIndex = password.index(password.startIndex, offsetBy: minCount - 1)
    let secondIndex = password.index(password.startIndex, offsetBy: maxCount - 1)

    if (password[firstIndex] == char && password[secondIndex] != char) || (password[firstIndex] != char && password[secondIndex] == char) {
        validCountPart2 += 1
    }
}

print(validCountPart1)
print(validCountPart2)