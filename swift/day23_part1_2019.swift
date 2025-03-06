
import Foundation

// MARK: - Intcode Computer

class IntcodeComputer {
    enum State {
        case running, halted, waitingForInput
    }
    
    var memory: [Int]
    var ip: Int = 0  // Instruction pointer
    var relativeBase: Int = 0
    var state: State = .running
    var inputs: [Int] = []
    var outputs: [Int] = []

    init(program: [Int], inputs: [Int] = []) {
        self.memory = program
        self.inputs = inputs
        // Expand memory
        self.memory.reserveCapacity(4096)
        for _ in program.count..<4096 {
            self.memory.append(0)
        }
    }
    
    func addInput(_ input: Int) {
        inputs.append(input)
    }

    func run() {
        state = .running
        
        while state == .running {
            let instruction = memory[ip]
            let opcode = instruction % 100
            let mode1 = (instruction / 100) % 10
            let mode2 = (instruction / 1000) % 10
            let mode3 = (instruction / 10000) % 10
            
            switch opcode {
            case 1: // Addition
                let param1 = getValue(mode: mode1, offset: 1)
                let param2 = getValue(mode: mode2, offset: 2)
                setValue(mode: mode3, offset: 3, value: param1 + param2)
                ip += 4
            case 2: // Multiplication
                let param1 = getValue(mode: mode1, offset: 1)
                let param2 = getValue(mode: mode2, offset: 2)
                setValue(mode: mode3, offset: 3, value: param1 * param2)
                ip += 4
            case 3: // Input
                if inputs.isEmpty {
                    state = .waitingForInput
                    return
                }
                let input = inputs.removeFirst()
                setValue(mode: mode1, offset: 1, value: input)
                ip += 2
            case 4: // Output
                let output = getValue(mode: mode1, offset: 1)
                outputs.append(output)
                ip += 2
            case 5: // Jump-if-true
                let param1 = getValue(mode: mode1, offset: 1)
                let param2 = getValue(mode: mode2, offset: 2)
                ip = (param1 != 0) ? param2 : ip + 3
            case 6: // Jump-if-false
                let param1 = getValue(mode: mode1, offset: 1)
                let param2 = getValue(mode: mode2, offset: 2)
                ip = (param1 == 0) ? param2 : ip + 3
            case 7: // Less than
                let param1 = getValue(mode: mode1, offset: 1)
                let param2 = getValue(mode: mode2, offset: 2)
                setValue(mode: mode3, offset: 3, value: (param1 < param2) ? 1 : 0)
                ip += 4
            case 8: // Equals
                let param1 = getValue(mode: mode1, offset: 1)
                let param2 = getValue(mode: mode2, offset: 2)
                setValue(mode: mode3, offset: 3, value: (param1 == param2) ? 1 : 0)
                ip += 4
            case 9: // Adjust relative base
                let param1 = getValue(mode: mode1, offset: 1)
                relativeBase += param1
                ip += 2
            case 99: // Halt
                state = .halted
            default:
                fatalError("Unknown opcode: \(opcode)")
            }
        }
    }
    
    private func getValue(mode: Int, offset: Int) -> Int {
        switch mode {
        case 0: // Position mode
            return memory[memory[ip + offset]]
        case 1: // Immediate mode
            return memory[ip + offset]
        case 2: // Relative mode
            return memory[relativeBase + memory[ip + offset]]
        default:
            fatalError("Unknown parameter mode: \(mode)")
        }
    }
    
    private func setValue(mode: Int, offset: Int, value: Int) {
        switch mode {
        case 0: // Position mode
            memory[memory[ip + offset]] = value
        case 2: // Relative mode
            memory[relativeBase + memory[ip + offset]] = value
        default:
            fatalError("Unknown parameter mode for setting value: \(mode)")
        }
    }
    
    func resetOutputs() {
         outputs = []
     }
}

// MARK: - Main Execution

func solve() {
    // Read the program from the file
    guard let input = try? String(contentsOfFile: "input.txt") else {
        fatalError("Could not read input file")
    }
    let program = input.trimmingCharacters(in: .whitespacesAndNewlines).split(separator: ",").compactMap { Int($0) }
    
    // Initialize 50 computers
    var computers: [IntcodeComputer] = []
    for i in 0..<50 {
        let computer = IntcodeComputer(program: program, inputs: [i])
        computers.append(computer)
    }
    
    var queues: [[(Int, Int)]] = Array(repeating: [], count: 50)  // Queues for each computer

    while true {
        var anyOutput = false
        for i in 0..<50 {
             let computer = computers[i]

             // Provide input if available, otherwise -1
             if queues[i].isEmpty {
                 computer.addInput(-1)
             } else {
                let (x, y) = queues[i].removeFirst()
                computer.addInput(x)
                computer.addInput(y)
             }

            computer.run()

            if !computer.outputs.isEmpty {
                anyOutput = true
                for j in stride(from: 0, to: computer.outputs.count, by: 3) {
                     let destination = computer.outputs[j]
                     let x = computer.outputs[j + 1]
                     let y = computer.outputs[j + 2]

                     if destination == 255 {
                         print(y)
                         return
                     }
                     if destination >= 0 && destination < 50 {
                          queues[destination].append((x, y))
                     }
                 }
                computer.resetOutputs()
            }
         }
        if !anyOutput { // Added to check for no-output scenario. Important if packets are not arriving.
                print("No network activity.")
                return
        }
    }
}


solve()
