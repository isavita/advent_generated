
import Foundation

func computeOperand(val: Int, a: Int, b: Int, c: Int) -> Int {
    switch val {
    case 0...3:
        return val
    case 4:
        return a
    case 5:
        return b
    case 6:
        return c
    default:
        fatalError("Invalid combo operand: \(val)")
    }
}

func simulateComputer(program: [Int], a: Int, b: Int, c: Int) -> [Int] {
    var outs: [Int] = []
    var currentA = a
    var currentB = b
    var currentC = c
    var i = 0
    let programLength = program.count

    while i < programLength {
        let cmd = program[i]
        i += 1

        switch cmd {
        case 0:
            currentA >>= computeOperand(val: program[i], a: currentA, b: currentB, c: currentC)
        case 1:
            currentB ^= program[i]
        case 2:
            currentB = computeOperand(val: program[i], a: currentA, b: currentB, c: currentC) % 8
        case 3:
            if currentA != 0 {
                i = program[i] - 1
            } else {
                i += 1
            }
        case 4:
            currentB ^= currentC
        case 5:
            outs.append(computeOperand(val: program[i], a: currentA, b: currentB, c: currentC) % 8)
        case 6:
            currentB = currentA >> computeOperand(val: program[i], a: currentA, b: currentB, c: currentC)
        case 7:
            currentC = currentA >> computeOperand(val: program[i], a: currentA, b: currentB, c: currentC)
        default:
            fatalError("Invalid opcode: \(cmd)")
        }

        if i < programLength {
            i += 1
        }
    }

    return outs
}

func check(program: [Int], initialB: Int, initialC: Int) -> [Int] {
    var valids: [Int] = []
    var stack: [(depth: Int, score: Int)] = [(0, 0)]
    var seen: Set<Int> = []

    while let (depth, score) = stack.popLast() {
        let key = depth * 1000 + score
        if seen.contains(key) {
            continue
        }
        seen.insert(key)

        if depth == program.count {
            valids.append(score)
        } else {
            for i in 0..<8 {
                let newScore = i + 8 * score
                let result = simulateComputer(program: program, a: newScore, b: initialB, c: initialC)
                if !result.isEmpty && result[0] == program[program.count - 1 - depth] {
                    stack.append((depth + 1, newScore))
                }
            }
        }
    }
    return valids
}

func main() {
    guard let input = try? String(contentsOfFile: "input.txt", encoding: .utf8) else {
        fatalError("Could not read input.txt")
    }

    var a = 0
    var b = 0
    var c = 0
    var program: [Int] = []

    for line in input.components(separatedBy: .newlines) {
        let trimmedLine = line.trimmingCharacters(in: .whitespaces)

        if trimmedLine.starts(with: "Register A:") {
            a = Int(trimmedLine.components(separatedBy: ":")[1].trimmingCharacters(in: .whitespaces))!
        } else if trimmedLine.starts(with: "Register B:") {
            b = Int(trimmedLine.components(separatedBy: ":")[1].trimmingCharacters(in: .whitespaces))!
        } else if trimmedLine.starts(with: "Register C:") {
            c = Int(trimmedLine.components(separatedBy: ":")[1].trimmingCharacters(in: .whitespaces))!
        } else if trimmedLine.starts(with: "Program:") {
            program = trimmedLine.components(separatedBy: ":")[1].trimmingCharacters(in: .whitespaces).components(separatedBy: ",").map { Int($0.trimmingCharacters(in: .whitespaces))! }
        }
    }

    let validValues = check(program: program, initialB: b, initialC: c)
    print(validValues.min()!)
}

main()
