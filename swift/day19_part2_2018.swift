
import Foundation

var instructions: [String: ((inout [Int], Int, Int) -> Int)] = [
    "addr": { r, a, b in r[a] + r[b] },
    "addi": { r, a, b in r[a] + b },
    "mulr": { r, a, b in r[a] * r[b] },
    "muli": { r, a, b in r[a] * b },
    "banr": { r, a, b in r[a] & r[b] },
    "bani": { r, a, b in r[a] & b },
    "borr": { r, a, b in r[a] | r[b] },
    "bori": { r, a, b in r[a] | b },
    "setr": { r, a, b in r[a] },
    "seti": { r, a, b in a },
    "gtir": { r, a, b in a > r[b] ? 1 : 0 },
    "gtri": { r, a, b in r[a] > b ? 1 : 0 },
    "gtrr": { r, a, b in r[a] > r[b] ? 1 : 0 },
    "eqir": { r, a, b in a == r[b] ? 1 : 0 },
    "eqri": { r, a, b in r[a] == b ? 1 : 0 },
    "eqrr": { r, a, b in r[a] == r[b] ? 1 : 0 }
]

func loadProgram(lines: [String]) -> (Int, [(inout [Int]) -> Void]) {
    var program: [(inout [Int]) -> Void] = []
    var ipRegister = 0
    let re = try! NSRegularExpression(pattern: "\\d+")

    for line in lines {
        if line.hasPrefix("#ip") {
            ipRegister = Int(line.components(separatedBy: " ")[1])!
            continue
        }

        let parts = line.components(separatedBy: " ")
        let op = instructions[parts[0]]!
        let nums = re.matches(in: line, range: NSRange(line.startIndex..., in: line)).map {
            Int((line as NSString).substring(with: $0.range))!
        }
        let a = nums[0]
        let b = nums[1]
        let c = nums[2]

        program.append { r in
            r[c] = op(&r, a, b)
        }
    }
    return (ipRegister, program)
}

func runProgram(ipRegister: Int, program: [(inout [Int]) -> Void], registers: inout [Int], maxCycles: Int) -> [Int] {
    var ip = 0
    var cycles = 0

    while ip >= 0 && ip < program.count {
        registers[ipRegister] = ip
        program[ip](&registers)
        ip = registers[ipRegister] + 1
        cycles += 1
        if maxCycles > 0 && cycles >= maxCycles {
            break
        }
    }
    return registers
}

func max(_ slice: [Int]) -> Int {
    var maxValue = slice[0]
    for v in slice {
        if v > maxValue {
            maxValue = v
        }
    }
    return maxValue
}

let fileURL = URL(fileURLWithPath: "input.txt")
let input = try String(contentsOf: fileURL)
let lines = input.components(separatedBy: .newlines).filter { !$0.isEmpty }

let (ipRegister, program) = loadProgram(lines: lines)

var registers = Array(repeating: 0, count: 6)
registers[0] = 1
registers = runProgram(ipRegister: ipRegister, program: program, registers: &registers, maxCycles: 1000)
let n = max(registers)
var total = 0
for i in 1...n {
    if n % i == 0 {
        total += i
    }
}
print(total)
