
import Foundation

func isRegister(_ x: String) -> Bool {
    return ["a", "b", "c", "d"].contains(x)
}

func getValue(_ x: String, _ registers: [String: Int]) -> Int {
    if isRegister(x) {
        return registers[x]!
    } else {
        return Int(x)!
    }
}

func executeProgram(_ instructions: inout [String], _ registers: inout [String: Int]) {
    var i = 0
    while i < instructions.count {
        if (i + 5 < instructions.count) {
            let pattern = Array(instructions[i..<i+6])
            if (pattern[0].hasPrefix("cpy") &&
                pattern[1].hasPrefix("inc") &&
                pattern[2].hasPrefix("dec") &&
                pattern[3].hasPrefix("jnz") &&
                pattern[4].hasPrefix("dec") &&
                pattern[5].hasPrefix("jnz")) {

                let cpyComponents = pattern[0].split(separator: " ")
                let incComponents = pattern[1].split(separator: " ")
                let decCComponents = pattern[2].split(separator: " ")
                let jnzCComponents = pattern[3].split(separator: " ")
                let decDComponents = pattern[4].split(separator: " ")
                let jnzDComponents = pattern[5].split(separator: " ")
                
                if (cpyComponents.count == 3 && incComponents.count == 2 && decCComponents.count == 2 &&
                    jnzCComponents.count == 3 && decDComponents.count == 2 && jnzDComponents.count == 3) {

                    let cpyX = String(cpyComponents[1])
                    let cpyY = String(cpyComponents[2])
                    let incA = String(incComponents[1])
                    let decC = String(decCComponents[1])
                    let jnzC = String(jnzCComponents[1])
                    let jnzCOffset = String(jnzCComponents[2])
                    let decD = String(decDComponents[1])
                    let jnzD = String(jnzDComponents[1])
                    let jnzDOffset = String(jnzDComponents[2])
                    
                    if (incA == "a" && decC == cpyY && jnzC == cpyY && Int(jnzCOffset) == -2 &&
                        decD == "d" && jnzD == "d" && Int(jnzDOffset) == -5) {
                        registers["a"]! += registers[cpyX]! * registers["d"]!
                        registers[cpyY] = 0
                        registers["d"] = 0
                        i += 6
                        continue
                    }
                }
            }
        }
        
        let parts = instructions[i].split(separator: " ").map { String($0) }
        let cmd = parts[0]

        if cmd == "tgl" {
            let x = getValue(parts[1], registers)
            let targetIdx = i + x
            if targetIdx >= 0 && targetIdx < instructions.count {
                var targetParts = instructions[targetIdx].split(separator: " ").map { String($0) }
                if targetParts.count == 2 {
                    if targetParts[0] == "inc" {
                        targetParts[0] = "dec"
                    } else {
                        targetParts[0] = "inc"
                    }
                } else if targetParts.count == 3 {
                    if targetParts[0] == "jnz" {
                        targetParts[0] = "cpy"
                    } else {
                        targetParts[0] = "jnz"
                    }
                }
                instructions[targetIdx] = targetParts.joined(separator: " ")
            }
            i += 1
            continue
        }

        if cmd == "cpy" {
            let x = parts[1]
            let y = parts[2]
            if isRegister(y) {
                registers[y] = getValue(x, registers)
            }
            i += 1
        } else if cmd == "inc" {
            let x = parts[1]
            if isRegister(x) {
                registers[x]! += 1
            }
            i += 1
        } else if cmd == "dec" {
            let x = parts[1]
            if isRegister(x) {
                registers[x]! -= 1
            }
            i += 1
        } else if cmd == "jnz" {
            let x = parts[1]
            let y = parts[2]
            if getValue(x, registers) != 0 {
                i += getValue(y, registers)
            } else {
                i += 1
            }
        } else {
            i += 1
        }
    }
}

func main() {
    guard let input = try? String(contentsOfFile: "input.txt", encoding: .utf8) else {
        return
    }
    var instructions = input.split(separator: "\n").map { String($0) }
    var registers = ["a": 12, "b": 0, "c": 0, "d": 0]
    executeProgram(&instructions, &registers)
    print(registers["a"]!)
}

main()
