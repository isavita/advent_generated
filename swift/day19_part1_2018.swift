
import Foundation

var registers = [0, 0, 0, 0, 0, 0]
var ip = 0

let input = try String(contentsOfFile: "input.txt")
let lines = input.components(separatedBy: .newlines)

let ipRegister = Int(lines[0].components(separatedBy: " ")[1])!

var instructions = Array(lines[1...])

while ip < instructions.count {
    let parts = instructions[ip].components(separatedBy: " ")
    let opcode = parts[0]
    let A = Int(parts[1])!
    let B = Int(parts[2])!
    let C = Int(parts[3])!

    registers[ipRegister] = ip

    switch opcode {
    case "addr":
        registers[C] = registers[A] + registers[B]
    case "addi":
        registers[C] = registers[A] + B
    case "mulr":
        registers[C] = registers[A] * registers[B]
    case "muli":
        registers[C] = registers[A] * B
    case "banr":
        registers[C] = registers[A] & registers[B]
    case "bani":
        registers[C] = registers[A] & B
    case "borr":
        registers[C] = registers[A] | registers[B]
    case "bori":
        registers[C] = registers[A] | B
    case "setr":
        registers[C] = registers[A]
    case "seti":
        registers[C] = A
    case "gtir":
        registers[C] = A > registers[B] ? 1 : 0
    case "gtri":
        registers[C] = registers[A] > B ? 1 : 0
    case "gtrr":
        registers[C] = registers[A] > registers[B] ? 1 : 0
    case "eqir":
        registers[C] = A == registers[B] ? 1 : 0
    case "eqri":
        registers[C] = registers[A] == B ? 1 : 0
    case "eqrr":
        registers[C] = registers[A] == registers[B] ? 1 : 0
    default:
        break
    }

    ip = registers[ipRegister]
    ip += 1
}

print(registers[0])
