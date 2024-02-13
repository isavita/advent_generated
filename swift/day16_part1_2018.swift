
import Foundation

struct OP {
    var a: Character
    var b: Character
    var action: Character
    var name: String
    var matchCount: [UInt8]
}

func remove(op: inout OP, c: UInt8) {
    if let index = op.matchCount.firstIndex(of: c) {
        op.matchCount.remove(at: index)
    }
}

func add(op: inout OP, c: UInt8) {
    if !op.matchCount.contains(c) {
        op.matchCount.append(c)
    }
}

func match(r: [Int], c: [Int]) -> Bool {
    if r.count != c.count {
        return false
    }
    for i in 0..<r.count {
        if r[i] != c[i] {
            return false
        }
    }
    return true
}

func runOp(op: OP, registers: [Int], instruction: [UInt8]) -> [Int] {
    var registerCP = registers
    var A, B: Int
    if op.a == "r" {
        A = registerCP[Int(instruction[1])]
    } else {
        A = Int(instruction[1])
    }
    if op.b == "r" {
        B = registerCP[Int(instruction[2])]
    } else {
        B = Int(instruction[2])
    }
    switch op.action {
    case "+":
        registerCP[Int(instruction[3])] = A + B
    case "*":
        registerCP[Int(instruction[3])] = A * B
    case "&":
        registerCP[Int(instruction[3])] = A & B
    case "|":
        registerCP[Int(instruction[3])] = A | B
    case "a":
        registerCP[Int(instruction[3])] = A
    case ">":
        if A > B {
            registerCP[Int(instruction[3])] = 1
        } else {
            registerCP[Int(instruction[3])] = 0
        }
    case "=":
        if A == B {
            registerCP[Int(instruction[3])] = 1
        } else {
            registerCP[Int(instruction[3])] = 0
        }
    default:
        print("not valid instruction")
    }
    return registerCP
}

func testCode(registers: [Int], n: [Int], instruction: [UInt8], opcodes: inout [OP]) -> Int {
    var sum = 0
    for i in 0..<opcodes.count {
        if match(r: n, c: runOp(op: opcodes[i], registers: registers, instruction: instruction)) {
            add(op: &opcodes[i], c: instruction[0])
            sum += 1
        }
    }
    return sum
}

func strToInt(_ s: String) -> Int {
    return Int(s) ?? 0
}

func regSplit(text: String, delimiter: String) -> [String] {
    let regex = try! NSRegularExpression(pattern: delimiter)
    let matches = regex.matches(in: text, range: NSRange(text.startIndex..., in: text))
    var lastStart = text.startIndex
    var result: [String] = []
    for match in matches {
        let range = lastStart..<text.index(text.startIndex, offsetBy: match.range.lowerBound)
        result.append(String(text[range]))
        lastStart = text.index(text.startIndex, offsetBy: match.range.upperBound)
    }
    let finalRange = lastStart..<text.endIndex
    result.append(String(text[finalRange]))
    return result
}

let fileURL = URL(fileURLWithPath: "input.txt")
let inputStr = try String(contentsOf: fileURL).trimmingCharacters(in: .whitespacesAndNewlines)
let lines = inputStr.components(separatedBy: "\n")

var opcodes: [OP] = [
    OP(a: "r", b: "r", action: "+", name: "addr", matchCount: []),
    OP(a: "r", b: "v", action: "+", name: "addi", matchCount: []),
    OP(a: "r", b: "r", action: "*", name: "mulr", matchCount: []),
    OP(a: "r", b: "v", action: "*", name: "muli", matchCount: []),
    OP(a: "r", b: "r", action: "&", name: "banr", matchCount: []),
    OP(a: "r", b: "v", action: "&", name: "bani", matchCount: []),
    OP(a: "r", b: "r", action: "|", name: "borr", matchCount: []),
    OP(a: "r", b: "v", action: "|", name: "bori", matchCount: []),
    OP(a: "r", b: "r", action: "a", name: "setr", matchCount: []),
    OP(a: "v", b: "r", action: "a", name: "seti", matchCount: []),
    OP(a: "v", b: "r", action: ">", name: "gtir", matchCount: []),
    OP(a: "r", b: "v", action: ">", name: "gtri", matchCount: []),
    OP(a: "r", b: "r", action: ">", name: "gtrr", matchCount: []),
    OP(a: "v", b: "r", action: "=", name: "eqir", matchCount: []),
    OP(a: "r", b: "v", action: "=", name: "eqri", matchCount: []),
    OP(a: "r", b: "r", action: "=", name: "eqir", matchCount: [])
]

var sum = 0
var lineCount = 0
while lineCount < lines.count {
    if !lines[lineCount].isEmpty && lines[lineCount].first == "B" {
        let split = regSplit(text: lines[lineCount], delimiter: "[^0-9]+")
        let registers = [
            strToInt(split[1]),
            strToInt(split[2]),
            strToInt(split[3]),
            strToInt(split[4])
        ]
        let instructionSplit = regSplit(text: lines[lineCount + 1], delimiter: "[^0-9]+")
        let instruction = [
            UInt8(strToInt(instructionSplit[0])),
            UInt8(strToInt(instructionSplit[1])),
            UInt8(strToInt(instructionSplit[2])),
            UInt8(strToInt(instructionSplit[3]))
        ]
        let nSplit = regSplit(text: lines[lineCount + 2], delimiter: "[^0-9]+")
        let n = [
            strToInt(nSplit[1]),
            strToInt(nSplit[2]),
            strToInt(nSplit[3]),
            strToInt(nSplit[4])
        ]
        let tempSum = testCode(registers: registers, n: n, instruction: instruction, opcodes: &opcodes)
        
        if tempSum >= 3 {
            sum += 1
        }
        
        lineCount += 4
    } else {
        break
    }
}

print(sum)
