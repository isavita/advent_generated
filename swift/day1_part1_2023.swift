import Foundation

let fileURL = URL(fileURLWithPath: "input.txt")

do {
    let input = try String(contentsOf: fileURL)
    var sum = 0

    input.enumerateLines { line, _ in
        if line.isEmpty {
            return
        }

        var firstDigit = -1
        var lastDigit = -1

        for char in line {
            if let digit = Int(String(char)), char.isNumber {
                if firstDigit == -1 {
                    firstDigit = digit
                }
                lastDigit = digit
            }
        }

        if firstDigit != -1, lastDigit != -1 {
            let value = Int("\(firstDigit)\(lastDigit)")!
            sum += value
        }
    }

    print(sum)
} catch {
    print("Error reading file:", error)
}