
import Foundation

func solve() {
    guard let data = try? String(contentsOfFile: "input.txt") else { return }
    let lines = data.components(separatedBy: "\n").map { $0.trimmingCharacters(in: .whitespaces) }.filter { !$0.isEmpty }
    var a = 0, b = 0, c = 0
    var program: [Int] = []

    for line in lines {
        if line.hasPrefix("Register A:") {
            a = Int(line.components(separatedBy: ":")[1].trimmingCharacters(in: .whitespaces)) ?? 0
        } else if line.hasPrefix("Register B:") {
            b = Int(line.components(separatedBy: ":")[1].trimmingCharacters(in: .whitespaces)) ?? 0
        } else if line.hasPrefix("Register C:") {
            c = Int(line.components(separatedBy: ":")[1].trimmingCharacters(in: .whitespaces)) ?? 0
        } else if line.hasPrefix("Program:") {
            program = line.components(separatedBy: ":")[1].trimmingCharacters(in: .whitespaces).components(separatedBy: ",").map { Int($0.trimmingCharacters(in: .whitespaces)) ?? 0 }
        }
    }

    func getComboVal(_ op: Int) -> Int {
        switch op {
        case 0...3: return op
        case 4: return a
        case 5: return b
        case 6: return c
        default: fatalError("Invalid combo operand")
        }
    }

    var outputVals: [String] = []
    var ip = 0
    while ip < program.count {
        let opcode = program[ip]
        guard ip + 1 < program.count else { break }
        let operand = program[ip + 1]

        switch opcode {
        case 0:
            let den = getComboVal(operand)
            a = den == 0 ? 0 : a >> den
            ip += 2
        case 1:
            b ^= operand
            ip += 2
        case 2:
            b = getComboVal(operand) % 8
            ip += 2
        case 3:
            ip = a != 0 ? operand : ip + 2
        case 4:
            b ^= c
            ip += 2
        case 5:
            outputVals.append("\(getComboVal(operand) % 8)")
            ip += 2
        case 6:
            b = a >> getComboVal(operand)
            ip += 2
        case 7:
            c = a >> getComboVal(operand)
            ip += 2
        default:
            break
        }
    }

    print(outputVals.joined(separator: ","))
}

solve()
