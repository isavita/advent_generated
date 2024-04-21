import Foundation

func main() {
    do {
        let fileContent = try String(contentsOfFile: "input.txt", encoding: .utf8)
        let instructions = fileContent.components(separatedBy: "\n")
        
        for a in 1... {
            if producesClockSignal(a, instructions) {
                print(a)
                break
            }
        }
    } catch {
        print("Error reading file")
    }
}

func producesClockSignal(_ a: Int, _ instructions: [String]) -> Bool {
    var registers: [String: Int] = ["a": a, "b": 0, "c": 0, "d": 0]
    var lastOutput: Int?
    var outputCount = 0
    var i = 0

    while i < instructions.count {
        let parts = instructions[i].components(separatedBy: " ")
        switch parts[0] {
        case "cpy":
            let val = getValue(from: parts[1], in: registers)
            registers[parts[2]] = val
        case "inc":
            registers[parts[1]]! += 1
        case "dec":
            registers[parts[1]]! -= 1
        case "jnz":
            let val = getValue(from: parts[1], in: registers)
            if val != 0 {
                if let jump = Int(parts[2]) {
                    i += jump - 1
                }
            }
        case "out":
            let val = getValue(from: parts[1], in: registers)
            if val != 0 && val != 1 {
                return false
            }
            if outputCount > 0 && val == lastOutput {
                return false
            }
            lastOutput = val
            outputCount += 1
            if outputCount > 50 {
                return true
            }
        default:
            break
        }
        i += 1
    }
    return false
}

func getValue(from str: String, in registers: [String: Int]) -> Int {
    if let val = Int(str) {
        return val
    } else {
        return registers[str]!
    }
}

main()