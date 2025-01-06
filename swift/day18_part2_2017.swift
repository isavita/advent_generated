
import Foundation

enum Value {
    case register(String)
    case number(Int)

    init(_ str: String) {
        if let num = Int(str) {
            self = .number(num)
        } else {
            self = .register(str)
        }
    }

    func getValue(registers: [String: Int]) -> Int {
        switch self {
        case .register(let name):
            return registers[name, default: 0]
        case .number(let num):
            return num
        }
    }
}

enum Instruction {
    case snd(Value)
    case set(String, Value)
    case add(String, Value)
    case mul(String, Value)
    case mod(String, Value)
    case rcv(String)
    case jgz(Value, Value)

    init(_ line: String) {
        let parts = line.split(separator: " ").map(String.init)
        let op = parts[0]
        let x = parts[1]
        let y = parts.count > 2 ? Value(parts[2]) : nil

        switch op {
        case "snd": self = .snd(Value(x))
        case "set": self = .set(x, y!)
        case "add": self = .add(x, y!)
        case "mul": self = .mul(x, y!)
        case "mod": self = .mod(x, y!)
        case "rcv": self = .rcv(x)
        case "jgz": self = .jgz(Value(x), y!)
        default: fatalError("Unknown instruction: \(op)")
        }
    }
}

func solvePart1(instructions: [Instruction]) -> Int {
    var registers = [String: Int]()
    var pc = 0
    var lastSound = 0

    while pc >= 0 && pc < instructions.count {
        let instruction = instructions[pc]
        switch instruction {
        case .snd(let x):
            lastSound = x.getValue(registers: registers)
        case .set(let x, let y):
            registers[x] = y.getValue(registers: registers)
        case .add(let x, let y):
            registers[x, default: 0] += y.getValue(registers: registers)
        case .mul(let x, let y):
            registers[x, default: 0] *= y.getValue(registers: registers)
        case .mod(let x, let y):
            registers[x, default: 0] %= y.getValue(registers: registers)
        case .rcv(let x):
            if registers[x, default: 0] != 0 {
                return lastSound
            }
        case .jgz(let x, let y):
            if x.getValue(registers: registers) > 0 {
                pc += y.getValue(registers: registers)
                continue
            }
        }
        pc += 1
    }
    return 0
}

func solvePart2(instructions: [Instruction]) -> Int {
    var registers = [[String: Int]](repeating: [:], count: 2)
    registers[0]["p"] = 0
    registers[1]["p"] = 1
    var pcs = [0, 0]
    var queues = [[Int]](repeating: [], count: 2)
    var sentCount = [0, 0]
    var waiting = [false, false]

    func execute(program: Int) -> Bool {
        let pc = pcs[program]
        if pc < 0 || pc >= instructions.count {
            return false
        }

        let instruction = instructions[pc]
        switch instruction {
        case .snd(let x):
            queues[1 - program].append(x.getValue(registers: registers[program]))
            sentCount[program] += 1
        case .set(let x, let y):
            registers[program][x] = y.getValue(registers: registers[program])
        case .add(let x, let y):
            registers[program][x, default: 0] += y.getValue(registers: registers[program])
        case .mul(let x, let y):
            registers[program][x, default: 0] *= y.getValue(registers: registers[program])
        case .mod(let x, let y):
            registers[program][x, default: 0] %= y.getValue(registers: registers[program])
        case .rcv(let x):
            if queues[program].isEmpty {
                waiting[program] = true
                return false
            } else {
                registers[program][x] = queues[program].removeFirst()
                waiting[program] = false
            }
        case .jgz(let x, let y):
            if x.getValue(registers: registers[program]) > 0 {
                pcs[program] += y.getValue(registers: registers[program])
                return true
            }
        }
        pcs[program] += 1
        return true
    }

    while !(waiting[0] && waiting[1]) && (execute(program: 0) || execute(program: 1)) {
        
    }

    return sentCount[1]
}

do {
    let input = try String(contentsOfFile: "input.txt")
    let instructions = input.trimmingCharacters(in: .whitespacesAndNewlines)
        .split(separator: "\n")
        .map(String.init)
        .map(Instruction.init)

    print(solvePart1(instructions: instructions))
    print(solvePart2(instructions: instructions))

} catch {
    print("Error reading input file: \(error)")
}
