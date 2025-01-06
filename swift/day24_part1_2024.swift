
import Foundation

struct Gate {
    let input1: String
    let input2: String
    let operation: String
    let output: String
}

func solve() throws {
    let fileURL = URL(fileURLWithPath: "input.txt")
    let content = try String(contentsOf: fileURL)
    let lines = content.components(separatedBy: "\n")

    var wires: [String: Int] = [:]
    var gates: [Gate] = []
    var parsingWires = true

    for line in lines {
        let trimmedLine = line.trimmingCharacters(in: .whitespaces)
        if trimmedLine.isEmpty {
            parsingWires = false
            continue
        }

        if parsingWires {
            let parts = trimmedLine.components(separatedBy: ":")
            if parts.count == 2, let value = Int(parts[1].trimmingCharacters(in: .whitespaces)) {
                wires[parts[0]] = value
            }
        } else {
            let parts = trimmedLine.components(separatedBy: " ")
            if parts.count == 5 {
                gates.append(Gate(input1: parts[0], input2: parts[2], operation: parts[1], output: parts[4]))
            }
        }
    }

    var remainingGates = gates
    while !remainingGates.isEmpty {
        var newRemainingGates: [Gate] = []
        var progress = false
        for gate in remainingGates {
            if let val1 = wires[gate.input1], let val2 = wires[gate.input2] {
                var outputVal = 0
                switch gate.operation {
                case "AND":
                    outputVal = (val1 == 1 && val2 == 1) ? 1 : 0
                case "OR":
                    outputVal = (val1 == 1 || val2 == 1) ? 1 : 0
                case "XOR":
                    outputVal = (val1 != val2) ? 1 : 0
                default:
                    break
                }
                wires[gate.output] = outputVal
                progress = true
            } else {
                newRemainingGates.append(gate)
            }
        }
        if !progress { return }
        remainingGates = newRemainingGates
    }

    let zWires = wires.filter { $0.key.hasPrefix("z") }
    let sortedZWires = zWires.sorted {
        let num1 = Int($0.key.dropFirst()) ?? 0
        let num2 = Int($1.key.dropFirst()) ?? 0
        return num1 < num2
    }

    let binaryString = String(sortedZWires.reversed().map { String($0.value) }.joined())
    if let decimalValue = Int(binaryString, radix: 2) {
        print(decimalValue)
    }
}

do {
    try solve()
} catch {
    print("Error: \(error)")
}
