
import Foundation

var registers = ["a": 0, "b": 0]
var instructions = try String(contentsOfFile: "input.txt").components(separatedBy: .newlines)

var i = 0
while i < instructions.count {
    let parts = instructions[i].components(separatedBy: " ")
    switch parts[0] {
    case "hlf":
        registers[parts[1]]! /= 2
        i += 1
    case "tpl":
        registers[parts[1]]! *= 3
        i += 1
    case "inc":
        registers[parts[1]]! += 1
        i += 1
    case "jmp":
        i += Int(parts[1])!
    case "jie":
        if registers[parts[1].replacingOccurrences(of: ",", with: "")]! % 2 == 0 {
            i += Int(parts[2])!
        } else {
            i += 1
        }
    case "jio":
        if registers[parts[1].replacingOccurrences(of: ",", with: "")]! == 1 {
            i += Int(parts[2])!
        } else {
            i += 1
        }
    default:
        break
    }
}

print(registers["b"]!)
