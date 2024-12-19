
import Foundation

enum Mode {
    case position
    case immediate
    case relative
}

enum Opcode: Int {
    case add = 1
    case mul = 2
    case input = 3
    case output = 4
    case jt = 5
    case jf = 6
    case lt = 7
    case eq = 8
    case rbo = 9
    case halt = 99
}

func decode(_ n: Int) -> (Opcode, [Mode]) {
    let op = Opcode(rawValue: n % 100)!
    var n = n / 100
    var modes: [Mode] = []
    for _ in 0..<3 {
        switch n % 10 {
        case 0:
            modes.append(.position)
        case 1:
            modes.append(.immediate)
        case 2:
            modes.append(.relative)
        default:
            fatalError("Unknown mode")
        }
        n /= 10
    }
    return (op, modes)
}

class Machine {
    var data: [Int: Int]
    var ip: Int
    var inChannel: [Int]
    var outChannel: [Int]
    var relbase: Int
    var inIndex: Int

    init(program: [Int], inChannel: [Int]) {
        self.data = [:]
        for (i, n) in program.enumerated() {
            self.data[i] = n
        }
        self.ip = 0
        self.inChannel = inChannel
        self.outChannel = []
        self.relbase = 0
        self.inIndex = 0
    }

    func get(_ i: Int, _ mo: Mode) -> Int {
        switch mo {
        case .immediate:
            return data[i, default: 0]
        case .position:
            return data[data[i, default: 0], default: 0]
        case .relative:
            return data[relbase + data[i, default: 0], default: 0]
        }
    }

    func set(_ i: Int, _ mo: Mode, _ val: Int) {
        switch mo {
        case .position:
            data[data[i, default: 0]] = val
        case .relative:
            data[relbase + data[i, default: 0]] = val
        default:
            fatalError("Unknown mode")
        }
    }

    func step() -> Bool {
        let (op, modes) = decode(data[ip, default: 0])
        switch op {
        case .add:
            let val = get(ip + 1, modes[0]) + get(ip + 2, modes[1])
            set(ip + 3, modes[2], val)
            ip += 4
        case .mul:
            let val = get(ip + 1, modes[0]) * get(ip + 2, modes[1])
            set(ip + 3, modes[2], val)
            ip += 4
        case .input:
            set(ip + 1, modes[0], inChannel[inIndex])
            inIndex += 1
            ip += 2
        case .output:
            outChannel.append(get(ip + 1, modes[0]))
            ip += 2
        case .jt:
            if get(ip + 1, modes[0]) != 0 {
                ip = get(ip + 2, modes[1])
            } else {
                ip += 3
            }
        case .jf:
            if get(ip + 1, modes[0]) == 0 {
                ip = get(ip + 2, modes[1])
            } else {
                ip += 3
            }
        case .lt:
            if get(ip + 1, modes[0]) < get(ip + 2, modes[1]) {
                set(ip + 3, modes[2], 1)
            } else {
                set(ip + 3, modes[2], 0)
            }
            ip += 4
        case .eq:
            if get(ip + 1, modes[0]) == get(ip + 2, modes[1]) {
                set(ip + 3, modes[2], 1)
            } else {
                set(ip + 3, modes[2], 0)
            }
            ip += 4
        case .rbo:
            relbase += get(ip + 1, modes[0])
            ip += 2
        case .halt:
            return false
        }
        return true
    }

    func run() {
        while step() {}
    }
}

func run(program: [Int], inChannel: [Int]) -> [Int] {
    let m = Machine(program: program, inChannel: inChannel)
    m.run()
    return m.outChannel
}

enum Tile: Int {
    case empty = 0
    case wall = 1
    case block = 2
    case paddle = 3
    case ball = 4
}

struct Point: Hashable {
    let x: Int
    let y: Int
}

func countBlocks(program: [Int]) -> Int {
    let out = run(program: program, inChannel: [])
    var grid: [Point: Tile] = [:]
    var i = 0
    while i < out.count {
        grid[Point(x: out[i], y: out[i+1])] = Tile(rawValue: out[i+2])!
        i += 3
    }
    return grid.values.filter { $0 == .block }.count
}

func readAll(filepath: String) -> String {
    do {
        return try String(contentsOfFile: filepath).trimmingCharacters(in: .whitespacesAndNewlines)
    } catch {
        fatalError("Error reading file")
    }
}

func toInt(_ s: String) -> Int {
    guard let n = Int(s) else {
        fatalError("Error converting to Int")
    }
    return n
}

let input = readAll(filepath: "input.txt")
let program = input.split(separator: ",").map { toInt(String($0)) }
print(countBlocks(program: program))
