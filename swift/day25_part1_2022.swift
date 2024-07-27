
import Foundation

// Function to convert SNAFU to Decimal
func snafuToDecimal(_ snafu: String) -> Int {
    let digits = Array(snafu.reversed())
    var decimalValue = 0
    for (index, char) in digits.enumerated() {
        let value: Int
        switch char {
        case "2":
            value = 2
        case "1":
            value = 1
        case "0":
            value = 0
        case "-":
            value = -1
        case "=":
            value = -2
        default:
            fatalError("Invalid SNAFU digit")
        }
        decimalValue += value * Int(pow(5.0, Double(index)))
    }
    return decimalValue
}

// Function to convert Decimal to SNAFU
func decimalToSnafu(_ decimal: Int) -> String {
    var number = decimal
    var snafu = ""
    
    while number != 0 {
        var remainder = number % 5
        number /= 5
        
        if remainder > 2 {
            remainder -= 5
            number += 1
        }
        
        switch remainder {
        case 2:
            snafu = "2" + snafu
        case 1:
            snafu = "1" + snafu
        case 0:
            snafu = "0" + snafu
        case -1:
            snafu = "-" + snafu
        case -2:
            snafu = "=" + snafu
        default:
            fatalError("Invalid remainder")
        }
    }
    
    return snafu.isEmpty ? "0" : snafu
}

// Read input from file
func readInput(from filename: String) -> [String] {
    do {
        let contents = try String(contentsOfFile: filename)
        return contents.split(separator: "\n").map { String($0) }
    } catch {
        print("Error reading file: \(error)")
        return []
    }
}

// Main execution
let snafuNumbers = readInput(from: "input.txt")
let totalDecimal = snafuNumbers.reduce(0) { $0 + snafuToDecimal($1) }
let snafuResult = decimalToSnafu(totalDecimal)

print("The SNAFU number to supply to Bob's console is: \(snafuResult)")
