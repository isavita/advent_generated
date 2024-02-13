
import Foundation

// Read input from file
let fileURL = URL(fileURLWithPath: "input.txt")
let input = try String(contentsOf: fileURL)
let program = input.components(separatedBy: ",").compactMap { Int($0) }

// Run the Intcode program in sensor boost mode
print(runIntcode(memory: program, input: 2))

func runIntcode(memory: [Int], input: Int) -> Int {
    var memory = memory
    var output = 0
    var ip = 0
    var relativeBase = 0

    while true {
        let opcode = memory[ip] % 100
        let modes = String(memory[ip] / 100)

        func getParam(_ offset: Int) -> Int {
            let mode = modes.count >= offset ? Int(String(modes[modes.index(modes.endIndex, offsetBy: -offset)])) ?? 0 : 0
            let param = memory[ip + offset]
            switch mode {
            case 0:
                return memory[param]
            case 1:
                return param
            case 2:
                return memory[relativeBase + param]
            default:
                fatalError("unknown parameter mode")
            }
        }

        func setParam(_ offset: Int, _ value: Int) {
            let mode = modes.count >= offset ? Int(String(modes[modes.index(modes.endIndex, offsetBy: -offset)])) ?? 0 : 0
            let param = memory[ip + offset]
            switch mode {
            case 0:
                if param >= memory.count {
                    memory += Array(repeating: 0, count: param - memory.count + 1)
                }
                memory[param] = value
            case 2:
                if relativeBase + param >= memory.count {
                    memory += Array(repeating: 0, count: relativeBase + param - memory.count + 1)
                }
                memory[relativeBase + param] = value
            default:
                fatalError("unknown parameter mode")
            }
        }

        switch opcode {
        case 1:
            setParam(3, getParam(1) + getParam(2))
            ip += 4
        case 2:
            setParam(3, getParam(1) * getParam(2))
            ip += 4
        case 3:
            setParam(1, input)
            ip += 2
        case 4:
            output = getParam(1)
            ip += 2
        case 5:
            if getParam(1) != 0 {
                ip = getParam(2)
            } else {
                ip += 3
            }
        case 6:
            if getParam(1) == 0 {
                ip = getParam(2)
            } else {
                ip += 3
            }
        case 7:
            setParam(3, getParam(1) < getParam(2) ? 1 : 0)
            ip += 4
        case 8:
            setParam(3, getParam(1) == getParam(2) ? 1 : 0)
            ip += 4
        case 9:
            relativeBase += getParam(1)
            ip += 2
        case 99:
            return output
        default:
            fatalError("unknown opcode: \(opcode)")
        }
    }
}
