
import Foundation

// Step 1: Read Input
let fileURL = URL(fileURLWithPath: "input.txt")
let input = try String(contentsOf: fileURL)
let lines = input.components(separatedBy: .newlines)

// Step 2: Initialize Registers
var registers: [String: Int] = [:]

// Step 3: Process Instructions
for line in lines {
    let parts = line.components(separatedBy: " ")
    let reg = parts[0]
    let op = parts[1]
    let amount = Int(parts[2])!
    let condReg = parts[4]
    let condOp = parts[5]
    let condVal = Int(parts[6])!

    // Check condition
    var cond = false
    switch condOp {
    case ">":
        cond = registers[condReg, default: 0] > condVal
    case ">=":
        cond = registers[condReg, default: 0] >= condVal
    case "<":
        cond = registers[condReg, default: 0] < condVal
    case "<=":
        cond = registers[condReg, default: 0] <= condVal
    case "==":
        cond = registers[condReg, default: 0] == condVal
    case "!=":
        cond = registers[condReg, default: 0] != condVal
    default:
        print("Invalid condition operator")
    }

    if cond {
        switch op {
        case "inc":
            registers[reg, default: 0] += amount
        case "dec":
            registers[reg, default: 0] -= amount
        default:
            print("Invalid operation")
        }
    }
}

// Step 4: Find Max Value
let maxValue = registers.values.max() ?? 0
print(maxValue)
