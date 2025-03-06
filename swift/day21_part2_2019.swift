
import Foundation

// MARK: - Intcode Computer

class IntcodeComputer {
    var memory: [Int]
    var ip: Int = 0
    var relativeBase: Int = 0
    var inputs: [Int] = []
    var outputs: [Int] = []
    var isHalted = false

    init(memory: [Int]) {
        self.memory = memory
    }

    func run() {
        while !isHalted {
            let opcode = memory[ip] % 100
            let mode1 = (memory[ip] / 100) % 10
            let mode2 = (memory[ip] / 1000) % 10
            let mode3 = (memory[ip] / 10000) % 10

            switch opcode {
            case 1: // Add
                let param1 = getValue(mode: mode1, offset: 1)
                let param2 = getValue(mode: mode2, offset: 2)
                setValue(mode: mode3, offset: 3, value: param1 + param2)
                ip += 4
            case 2: // Multiply
                let param1 = getValue(mode: mode1, offset: 1)
                let param2 = getValue(mode: mode2, offset: 2)
                setValue(mode: mode3, offset: 3, value: param1 * param2)
                ip += 4
            case 3: // Input
                if inputs.isEmpty {
                    return // Wait for input
                }
                let input = inputs.removeFirst()
                setValue(mode: mode1, offset: 1, value: input)
                ip += 2
            case 4: // Output
                let param1 = getValue(mode: mode1, offset: 1)
                outputs.append(param1)
                ip += 2
            case 5: // Jump-if-true
                let param1 = getValue(mode: mode1, offset: 1)
                let param2 = getValue(mode: mode2, offset: 2)
                ip = param1 != 0 ? param2 : ip + 3
            case 6: // Jump-if-false
                let param1 = getValue(mode: mode1, offset: 1)
                let param2 = getValue(mode: mode2, offset: 2)
                ip = param1 == 0 ? param2 : ip + 3
            case 7: // Less than
                let param1 = getValue(mode: mode1, offset: 1)
                let param2 = getValue(mode: mode2, offset: 2)
                setValue(mode: mode3, offset: 3, value: param1 < param2 ? 1 : 0)
                ip += 4
            case 8: // Equals
                let param1 = getValue(mode: mode1, offset: 1)
                let param2 = getValue(mode: mode2, offset: 2)
                setValue(mode: mode3, offset: 3, value: param1 == param2 ? 1 : 0)
                ip += 4
            case 9: // Adjust relative base
                let param1 = getValue(mode: mode1, offset: 1)
                relativeBase += param1
                ip += 2
            case 99: // Halt
                isHalted = true
            default:
                fatalError("Invalid opcode: \(opcode)")
            }
        }
    }

    private func getValue(mode: Int, offset: Int) -> Int {
        switch mode {
        case 0: // Position mode
            let address = memory[ip + offset]
            return memory.indices.contains(address) ? memory[address] : 0
        case 1: // Immediate mode
            return memory[ip + offset]
        case 2: // Relative mode
            let address = memory[ip + offset] + relativeBase
            return memory.indices.contains(address) ? memory[address] : 0
        default:
            fatalError("Invalid parameter mode: \(mode)")
        }
    }

    private func setValue(mode: Int, offset: Int, value: Int) {
        switch mode {
        case 0: // Position mode
            let address = memory[ip + offset]
            if memory.indices.contains(address) {
                  memory[address] = value
            } else {
                memory.append(contentsOf: Array(repeating: 0, count: address - memory.count + 1))
                memory[address] = value
            }
          
        case 2: // Relative mode
            let address = memory[ip + offset] + relativeBase
             if memory.indices.contains(address) {
                  memory[address] = value
            } else {
                memory.append(contentsOf: Array(repeating: 0, count: address - memory.count + 1))
                memory[address] = value
            }
        default:
            fatalError("Invalid parameter mode for set: \(mode)")
        }
    }
}

// MARK: - Springdroid

func runSpringdroid(program: [Int], springscript: [String]) -> Int {
    let computer = IntcodeComputer(memory: program)

    // Convert springscript to ASCII and add to inputs
    for line in springscript {
        for char in line {
            computer.inputs.append(Int(char.asciiValue!))
        }
        computer.inputs.append(10) // Newline
    }

    computer.run()
   
    if let lastOutput = computer.outputs.last, lastOutput > 127 {
         return lastOutput
    } else {
        print("SpringDroid output")
        var outputStr = ""
        for val in computer.outputs{
            if let scalar = UnicodeScalar(val) {
                outputStr.append(Character(scalar))
            }
           
        }
         print(outputStr)
        return -1 // Indicate failure if the droid fell
    }
    
}

func part1(program: [Int]) -> Int {
    let springscript = [
        "NOT A J", // If A is hole, J = true
        "NOT B T", // If B is hole, T = true
        "OR T J",  // if A or B is hole, J = true
        "NOT C T", // If C is hole, T = true
        "OR T J",   // If A,B,C is hole J = true
        "AND D J",  //If D is ground and any of ABC are hole J=true
        "WALK"
    ]
    return runSpringdroid(program: program, springscript: springscript)
}

func part2(program: [Int]) -> Int {
    let springscript = [
        "NOT A J",
        "NOT B T",
        "OR T J",
        "NOT C T",
        "OR T J",
        "AND D J",          // J = (A or B or C) and D
        "NOT J T", // T = J
        "OR E T",
        "OR H T",
        "AND T J",
        "RUN"
    ]
    return runSpringdroid(program: program, springscript: springscript)
}


// MARK: - Main

func main() {
    guard let input = try? String(contentsOfFile: "input.txt") else {
        print("Failed to read input file.")
        return
    }
    
    let program = input.trimmingCharacters(in: .whitespacesAndNewlines)
        .components(separatedBy: ",")
        .compactMap { Int($0) }

    let hullDamage1 = part1(program: program)
    print("Part 1: Hull damage: \(hullDamage1)")

    let hullDamage2 = part2(program: program)
    print("Part 2: Hull damage: \(hullDamage2)")
}

main()
