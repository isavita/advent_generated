import Foundation

var wires: [String: UInt16] = [:]
var instructions: [(String, String)] = []

if let input = try? String(contentsOfFile: "input.txt") {
    let lines = input.components(separatedBy: .newlines)
    for line in lines {
        let parts = line.components(separatedBy: " -> ")
        instructions.append((parts[0], parts[1]))
    }
}

func getValue(_ input: String) -> UInt16 {
    if let value = UInt16(input) {
        return value
    } else {
        if let wireValue = wires[input] {
            return wireValue
        } else {
            let instruction = instructions.first { $0.1 == input }!
            let result = calculateValue(instruction.0)
            wires[input] = result
            return result
        }
    }
}

func calculateValue(_ instruction: String) -> UInt16 {
    let parts = instruction.components(separatedBy: " ")

    switch parts.count {
    case 1:
        return getValue(parts[0])
    case 2:
        return ~getValue(parts[1])
    case 3:
        let value1 = getValue(parts[0])
        let value2 = getValue(parts[2])

        switch parts[1] {
        case "AND":
            return value1 & value2
        case "OR":
            return value1 | value2
        case "LSHIFT":
            return value1 << value2
        case "RSHIFT":
            return value1 >> value2
        default:
            fatalError("Invalid instruction")
        }
    default:
        fatalError("Invalid instruction")
    }
}

let result = getValue("a")
print(result)