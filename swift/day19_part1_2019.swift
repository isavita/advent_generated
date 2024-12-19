
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
            modes.append(Mode(rawValue: modeSection % 10) ?? .position)
            modeSection /= 10
        }
        return modes
    }
}

class VM {
    var code: [Int: Int]
    var ip = 0
    var relativeBase = 0
    
    init(filename: String) {
        code = [:]
        load(filename: filename)
    }
    
    func load(filename: String) {
        guard let fileContent = try? String(contentsOfFile: filename),
              let listStr = fileContent.trimmingCharacters(in: .whitespacesAndNewlines).components(separatedBy: ",").map({Int($0)}) as? [Int]
        else {
            fatalError("Failed to load file")
        }
        
        for (i, value) in listStr.enumerated() {
            code[i] = value
        }
        ip = 0
        relativeBase = 0
    }
    
    func run(input: [Int]) -> [Int] {
        var inputIndex = 0
        var output = [Int]()
        
        while true {
            let cmd = Cmd(value: code[ip] ?? 0)
            
            switch cmd.opCode() {
            case 1:
                let params = getParamsAddresses(pos: ip, cmd: cmd, arity: 3)
                code[params[2]] = (code[params[0]] ?? 0) + (code[params[1]] ?? 0)
                ip += 4
            case 2:
                let params = getParamsAddresses(pos: ip, cmd: cmd, arity: 3)
                code[params[2]] = (code[params[0]] ?? 0) * (code[params[1]] ?? 0)
                ip += 4
            case 3:
                let params = getParamsAddresses(pos: ip, cmd: cmd, arity: 1)
                code[params[0]] = input[inputIndex]
                inputIndex += 1
                ip += 2
            case 4:
                let params = getParamsAddresses(pos: ip, cmd: cmd, arity: 1)
                output.append(code[params[0]] ?? 0)
                ip += 2
            case 5:
                let params = getParamsAddresses(pos: ip, cmd: cmd, arity: 2)
                if (code[params[0]] ?? 0) != 0 {
                    ip = code[params[1]] ?? 0
                } else {
                    ip += 3
                }
            case 6:
                let params = getParamsAddresses(pos: ip, cmd: cmd, arity: 2)
                if (code[params[0]] ?? 0) == 0 {
                    ip = code[params[1]] ?? 0
                } else {
                    ip += 3
                }
            case 7:
                let params = getParamsAddresses(pos: ip, cmd: cmd, arity: 3)
                code[params[2]] = (code[params[0]] ?? 0) < (code[params[1]] ?? 0) ? 1 : 0
                ip += 4
            case 8:
                let params = getParamsAddresses(pos: ip, cmd: cmd, arity: 3)
                code[params[2]] = (code[params[0]] ?? 0) == (code[params[1]] ?? 0) ? 1 : 0
                ip += 4
            case 9:
                let params = getParamsAddresses(pos: ip, cmd: cmd, arity: 1)
                relativeBase += (code[params[0]] ?? 0)
                ip += 2
            case 99:
                return output
            default:
                fatalError("Invalid opcode: \(cmd.opCode())")
            }
        }
    }
    
    private func getParamsAddresses(pos: Int, cmd: Cmd, arity: Int) -> [Int] {
        let modes = cmd.modes(arity: arity)
        var addresses = [Int]()
        for i in 0..<arity {
            addresses.append(getParamAddress(pos: pos + i + 1, mode: modes[i]))
        }
        return addresses
    }
    
    private func getParamAddress(pos: Int, mode: Mode) -> Int {
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

func beam(x: Int, y: Int) -> Bool {
    let vm = VM(filename: "input.txt")
    let output = vm.run(input: [x, y])
    return output.first == 1
}

var sum = 0
for y in 0..<50 {
    for x in 0..<50 {
        if beam(x: x, y: y) {
            sum += 1
        }
    }
}
print(sum)
