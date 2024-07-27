
import Foundation

func applyMask(value: UInt64, mask: String) -> UInt64 {
    var maskedValue = value
    for (index, char) in mask.enumerated() {
        let bitPosition = 35 - index // 0 is the least significant bit
        let bitValue = (maskedValue >> bitPosition) & 1
        
        switch char {
        case "0":
            maskedValue &= ~(1 << bitPosition) // Set bit to 0
        case "1":
            maskedValue |= (1 << bitPosition) // Set bit to 1
        case "X":
            maskedValue |= (bitValue << bitPosition) // Leave unchanged
        default:
            break
        }
    }
    return maskedValue
}

func main() {
    let fileURL = URL(fileURLWithPath: "input.txt")
    guard let input = try? String(contentsOf: fileURL) else {
        print("Failed to read input file.")
        return
    }

    var memory: [Int: UInt64] = [:]
    var currentMask: String = ""

    for line in input.split(separator: "\n") {
        if line.hasPrefix("mask = ") {
            currentMask = String(line.split(separator: "=")[1].trimmingCharacters(in: .whitespaces))
        } else if line.hasPrefix("mem[") {
            let parts = line.split(separator: "=").map { $0.trimmingCharacters(in: .whitespaces) }
            let address = Int(parts[0].dropFirst(4).dropLast(1))!
            let value = UInt64(parts[1])!
            let maskedValue = applyMask(value: value, mask: currentMask)
            memory[address] = maskedValue
        }
    }

    let sum = memory.values.reduce(0, +)
    print("Sum of all values in memory: \(sum)")
}

main()
