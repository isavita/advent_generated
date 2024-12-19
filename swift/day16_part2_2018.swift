
import Foundation

struct OP {
    let a: Character
    let b: Character
    let action: Character
    let name: String
    var matchCount: [UInt8] = []
}

func remove(op: inout OP, c: UInt8) {
    op.matchCount.removeAll { $0 == c }
}

func add(op: inout OP, c: UInt8) {
    if !op.matchCount.contains(c) {
        op.matchCount.append(c)
    }
}

func match(r: [Int], c: [Int]) -> Bool {
    guard r.count == c.count else { return false }
    return r.elementsEqual(c)
}

func runOp(op: OP, registers: [Int], instruction: [UInt8]) -> [Int] {
    var registerCP = registers
    let A: Int
    let B: Int
    
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
        registerCP[Int(instruction[3])] = A > B ? 1 : 0
    case "=":
        registerCP[Int(instruction[3])] = A == B ? 1 : 0
    default:
        print("not valid instruction")
    }
    return registerCP
}

func testCode(registers: [Int], result: [Int], instruction: [UInt8], opcodes: inout [OP]) -> Int {
    var sum = 0
    for i in 0..<opcodes.count {
        if match(r: result, c: runOp(op: opcodes[i], registers: registers, instruction: instruction)) {
            add(op: &opcodes[i], c: instruction[0])
            sum += 1
        }
    }
    return sum
}

func regSplit(text: String, delimeter: String) -> [String] {
    let regex = try! NSRegularExpression(pattern: delimeter)
    let matches = regex.matches(in: text, range: NSRange(text.startIndex..., in: text))
    var result: [String] = []
    var lastStart = text.startIndex
    for match in matches {
        result.append(String(text[lastStart..<text.index(text.startIndex, offsetBy: match.range.location)]))
        lastStart = text.index(text.startIndex, offsetBy: match.range.location + match.range.length)
    }
    result.append(String(text[lastStart..<text.endIndex]))
    return result
}

func strToInt(_ s: String) -> Int {
    return Int(s)!
}

func main() {
    guard let input = try? String(contentsOfFile: "input.txt") else {
        fatalError("Failed to read input.txt")
    }
    let lines = input.trimmingCharacters(in: .whitespacesAndNewlines).components(separatedBy: "\n")
    
    var opcodes = [
        OP(a: "r", b: "r", action: "+", name: "addr"),
        OP(a: "r", b: "v", action: "+", name: "addi"),
        OP(a: "r", b: "r", action: "*", name: "mulr"),
        OP(a: "r", b: "v", action: "*", name: "muli"),
        OP(a: "r", b: "r", action: "&", name: "banr"),
        OP(a: "r", b: "v", action: "&", name: "bani"),
        OP(a: "r", b: "r", action: "|", name: "borr"),
        OP(a: "r", b: "v", action: "|", name: "bori"),
        OP(a: "r", b: "r", action: "a", name: "setr"),
        OP(a: "v", b: "r", action: "a", name: "seti"),
        OP(a: "v", b: "r", action: ">", name: "gtir"),
        OP(a: "r", b: "v", action: ">", name: "gtri"),
        OP(a: "r", b: "r", action: ">", name: "gtrr"),
        OP(a: "v", b: "r", action: "=", name: "eqir"),
        OP(a: "r", b: "v", action: "=", name: "eqri"),
        OP(a: "r", b: "r", action: "=", name: "eqrr")
    ]
    
    var sum = 0
    var lineCount = 0
    while lineCount < lines.count {
        if lines[lineCount].starts(with: "B") {
            let split1 = regSplit(text: lines[lineCount], delimeter: "[^0-9]+")
            let registers = [
                strToInt(split1[1]),
                strToInt(split1[2]),
                strToInt(split1[3]),
                strToInt(split1[4])
            ]
            let split2 = regSplit(text: lines[lineCount + 1], delimeter: "[^0-9]+")
            let instruction = [
                UInt8(strToInt(split2[0])),
                UInt8(strToInt(split2[1])),
                UInt8(strToInt(split2[2])),
                UInt8(strToInt(split2[3]))
            ]
            let split3 = regSplit(text: lines[lineCount + 2], delimeter: "[^0-9]+")
            let result = [
                strToInt(split3[1]),
                strToInt(split3[2]),
                strToInt(split3[3]),
                strToInt(split3[4])
            ]
            
            if testCode(registers: registers, result: result, instruction: instruction, opcodes: &opcodes) >= 3 {
                sum += 1
            }
            lineCount += 4
        } else {
            break
        }
    }
    
    var orderedOpCodes: [UInt8: OP] = [:]
    
    while orderedOpCodes.count < 16 {
        for i in 0..<opcodes.count {
            if opcodes[i].matchCount.count == 1 {
                let c = opcodes[i].matchCount[0]
                orderedOpCodes[c] = opcodes[i]
                for j in 0..<opcodes.count {
                    remove(op: &opcodes[j], c: c)
                }
            }
        }
    }
    
    lineCount += 2
    var r = [0, 0, 0, 0]
    
    while lineCount < lines.count {
        let split = regSplit(text: lines[lineCount], delimeter: "[^0-9]+")
        let instruction = [
            UInt8(strToInt(split[0])),
            UInt8(strToInt(split[1])),
            UInt8(strToInt(split[2])),
            UInt8(strToInt(split[3]))
        ]
        
        r = runOp(op: orderedOpCodes[instruction[0]]!, registers: r, instruction: instruction)
        lineCount += 1
    }
    
    print(r[0])
}

main()
