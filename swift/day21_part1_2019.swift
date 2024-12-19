
import Foundation

enum Mode: Int {
    case position, immediate, relative
}

struct Cmd {
    let value: Int

    func opCode() -> Int {
        value % 100
    }

    func modes(arity: Int) -> [Mode] {
        var modeSection = value / 100
        var modes = [Mode]()
        for _ in 0..<arity {
            modes.append(Mode(rawValue: modeSection % 10)!)
            modeSection /= 10
        }
        return modes
    }
}

class VM {
    var code: [Int: Int] = [:]
    var ip = 0
    var relativeBase = 0
    var input: [Int] = []
    var output: [Int] = []

    init(filename: String) {
        load(filename: filename)
    }

    func load(filename: String) {
        guard let fileContent = try? String(contentsOfFile: filename),
              let trimmedContent = fileContent.trimmingCharacters(in: .whitespacesAndNewlines) as String?
        else {
            fatalError("Failed to read file")
        }
        let listStr = trimmedContent.components(separatedBy: ",")
        for (i, str) in listStr.enumerated() {
            if let value = Int(str) {
                code[i] = value
            } else {
                fatalError("Invalid input")
            }
        }
        ip = 0
        relativeBase = 0
    }

    func run() {
        var arity: Int
        while true {
            let cmd = Cmd(value: code[ip] ?? 0)
            switch cmd.opCode() {
            case 1:
                arity = 3
                let params = getParamsAddresses(pos: ip, cmd: cmd, arity: arity)
                code[params[2]] = (code[params[0]] ?? 0) + (code[params[1]] ?? 0)
            case 2:
                arity = 3
                let params = getParamsAddresses(pos: ip, cmd: cmd, arity: arity)
                code[params[2]] = (code[params[0]] ?? 0) * (code[params[1]] ?? 0)
            case 3:
                arity = 1
                let params = getParamsAddresses(pos: ip, cmd: cmd, arity: arity)
                code[params[0]] = input.removeFirst()
            case 4:
                arity = 1
                let params = getParamsAddresses(pos: ip, cmd: cmd, arity: arity)
                output.append(code[params[0]] ?? 0)
            case 5:
                arity = 2
                let params = getParamsAddresses(pos: ip, cmd: cmd, arity: arity)
                if (code[params[0]] ?? 0) != 0 {
                    ip = code[params[1]] ?? 0
                    continue
                }
            case 6:
                arity = 2
                let params = getParamsAddresses(pos: ip, cmd: cmd, arity: arity)
                if (code[params[0]] ?? 0) == 0 {
                    ip = code[params[1]] ?? 0
                    continue
                }
            case 7:
                arity = 3
                let params = getParamsAddresses(pos: ip, cmd: cmd, arity: arity)
                code[params[2]] = (code[params[0]] ?? 0) < (code[params[1]] ?? 0) ? 1 : 0
            case 8:
                arity = 3
                let params = getParamsAddresses(pos: ip, cmd: cmd, arity: arity)
                code[params[2]] = (code[params[0]] ?? 0) == (code[params[1]] ?? 0) ? 1 : 0
            case 9:
                arity = 1
                let params = getParamsAddresses(pos: ip, cmd: cmd, arity: arity)
                relativeBase += (code[params[0]] ?? 0)
            case 99:
                return
            default:
                fatalError("Invalid opcode: \(cmd.opCode())")
            }
            ip += arity + 1
        }
    }

    func getParamsAddresses(pos: Int, cmd: Cmd, arity: Int) -> [Int] {
        let modes = cmd.modes(arity: arity)
        var results = [Int]()
        for i in 0..<arity {
            results.append(getParamAddress(pos: pos + i + 1, mode: modes[i]))
        }
        return results
    }

    func getParamAddress(pos: Int, mode: Mode) -> Int {
        switch mode {
        case .position:
            return code[pos] ?? 0
        case .immediate:
            return pos
        case .relative:
            return relativeBase + (code[pos] ?? 0)
        }
    }
}

func sendString(vm: VM, s: String) {
    for char in s {
        vm.input.append(Int(char.asciiValue!))
    }
    vm.input.append(10)
}

func reader(vm: VM) {
    for c in vm.output {
        if c > 127 {
            print(c)
            return
        }
    }
}

let vm = VM(filename: "input.txt")

let instructions = [
    "NOT A J",
    "NOT B T",
    "OR T J",
    "NOT C T",
    "OR T J",
    "AND D J",
    "WALK",
]

for i in instructions {
    sendString(vm: vm, s: i)
}

vm.run()
reader(vm: vm)
