import Foundation

func getValue(_ arg: String, registers: [String: Int]) -> Int {
    if let val = Int(arg) {
        return val
    }
    return registers[arg] ?? 0
}

func main() {
    do {
        let fileContent = try String(contentsOfFile: "input.txt")
        let instructions = fileContent.components(separatedBy: "\n").map { $0.components(separatedBy: " ") }
        
        var registers: [String: Int] = [:]
        var lastSound = 0
        var i = 0
        
        while i < instructions.count {
            let instruction = instructions[i]
            let cmd = instruction[0]
            let arg1 = instruction[1]
            
            switch cmd {
            case "snd":
                lastSound = getValue(arg1, registers: registers)
            case "set":
                registers[arg1] = getValue(instruction[2], registers: registers)
            case "add":
                registers[arg1, default: 0] += getValue(instruction[2], registers: registers)
            case "mul":
                registers[arg1, default: 0] *= getValue(instruction[2], registers: registers)
            case "mod":
                registers[arg1, default: 0] %= getValue(instruction[2], registers: registers)
            case "rcv":
                if getValue(arg1, registers: registers) != 0 {
                    print(lastSound)
                    return
                }
            case "jgz":
                if getValue(arg1, registers: registers) > 0 {
                    i += getValue(instruction[2], registers: registers) - 1
                }
            default:
                break
            }
            i += 1
        }
    } catch {
        print("Error reading file")
    }
}

main()