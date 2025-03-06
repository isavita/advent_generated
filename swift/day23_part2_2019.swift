
import Foundation

// MARK: - Intcode Computer

class IntcodeComputer {
    var memory: [Int]
    var ip: Int = 0
    var relativeBase: Int = 0
    var inputs: [Int] = []
    var outputs: [Int] = []
    var isHalted: Bool = false
    var isWaitingForInput: Bool = false

    init(memory: [Int]) {
        self.memory = memory
    }

    func run() {
        while ip < memory.count {
            let instruction = memory[ip] % 100
            let modes = String(String(memory[ip]).dropLast(2).reversed())
            let parameterModes = modes.padding(toLength: 3, withPad: "0", startingAt: 0)

            switch instruction {
            case 1: // Add
                let param1 = getValue(at: ip + 1, mode: parameterModes[0])
                let param2 = getValue(at: ip + 2, mode: parameterModes[1])
                setValue(param1 + param2, at: ip + 3, mode: parameterModes[2])
                ip += 4
            case 2: // Multiply
                let param1 = getValue(at: ip + 1, mode: parameterModes[0])
                let param2 = getValue(at: ip + 2, mode: parameterModes[1])
                setValue(param1 * param2, at: ip + 3, mode: parameterModes[2])
                ip += 4
            case 3: // Input
                if inputs.isEmpty {
                    isWaitingForInput = true
                    return // Pause execution, waiting for input.
                }
                isWaitingForInput = false
                let input = inputs.removeFirst()
                setValue(input, at: ip + 1, mode: parameterModes[0])
                ip += 2
            case 4: // Output
                let param1 = getValue(at: ip + 1, mode: parameterModes[0])
                outputs.append(param1)
                ip += 2
            case 5: // Jump-if-true
                let param1 = getValue(at: ip + 1, mode: parameterModes[0])
                let param2 = getValue(at: ip + 2, mode: parameterModes[1])
                ip = param1 != 0 ? param2 : ip + 3
            case 6: // Jump-if-false
                let param1 = getValue(at: ip + 1, mode: parameterModes[0])
                let param2 = getValue(at: ip + 2, mode: parameterModes[1])
                ip = param1 == 0 ? param2 : ip + 3
            case 7: // Less than
                let param1 = getValue(at: ip + 1, mode: parameterModes[0])
                let param2 = getValue(at: ip + 2, mode: parameterModes[1])
                setValue(param1 < param2 ? 1 : 0, at: ip + 3, mode: parameterModes[2])
                ip += 4
            case 8: // Equals
                let param1 = getValue(at: ip + 1, mode: parameterModes[0])
                let param2 = getValue(at: ip + 2, mode: parameterModes[1])
                setValue(param1 == param2 ? 1 : 0, at: ip + 3, mode: parameterModes[2])
                ip += 4
            case 9: // Adjust relative base
                let param1 = getValue(at: ip + 1, mode: parameterModes[0])
                relativeBase += param1
                ip += 2
            case 99: // Halt
                isHalted = true
                return
            default:
                fatalError("Invalid opcode: \(instruction)")
            }
        }
    }
    
    private func getValue(at index: Int, mode: Character) -> Int {
        switch mode {
        case "0": // Position mode
            let address = memory[index]
            ensureMemorySize(address + 1)
            return memory[address]
        case "1": // Immediate mode
            ensureMemorySize(index + 1)
            return memory[index]
        case "2": // Relative mode
            let address = memory[index] + relativeBase
            ensureMemorySize(address + 1)
            return memory[address]
        default:
            fatalError("Invalid parameter mode: \(mode)")
        }
    }

    private func setValue(_ value: Int, at index: Int, mode: Character) {
        switch mode {
        case "0": // Position mode
            let address = memory[index]
            ensureMemorySize(address + 1)
            memory[address] = value
        case "2": // Relative mode
            let address = memory[index] + relativeBase
            ensureMemorySize(address + 1)
            memory[address] = value
        default:
            fatalError("Invalid parameter mode for set: \(mode)")
        }
    }

    private func ensureMemorySize(_ size: Int) {
        if size > memory.count {
            memory.append(contentsOf: [Int](repeating: 0, count: size - memory.count))
        }
    }
}

extension String {
    subscript(i: Int) -> Character {
        return self[index(startIndex, offsetBy: i)]
    }
}

// MARK: - Network Simulation
func simulateNetwork(program: [Int], part: Int) -> Int {
    let numComputers = 50
    var computers: [IntcodeComputer] = []
    var messageQueues: [[(x: Int, y: Int)]] = Array(repeating: [], count: numComputers)
    
    // Initialize computers
    for i in 0..<numComputers {
        let computer = IntcodeComputer(memory: program)
        computer.inputs.append(i) // Set network address
        computers.append(computer)
    }
    
    var natPacket: (x: Int, y: Int)? = nil
    var lastSentY: Int? = nil
    
    while true {
        var allIdle = true
        
        for i in 0..<numComputers {
            let computer = computers[i]
            
            // Handle Input
            if computer.isWaitingForInput || computer.inputs.isEmpty {
                if messageQueues[i].isEmpty {
                    computer.inputs.append(-1) // No packets, provide -1
                } else {
                    let (x, y) = messageQueues[i].removeFirst()
                    computer.inputs.append(contentsOf: [x, y])
                }
            }
            
            computer.run()  //Run the computer.
            
            // Handle Output
            while computer.outputs.count >= 3 {
                let address = computer.outputs.removeFirst()
                let x = computer.outputs.removeFirst()
                let y = computer.outputs.removeFirst()
                
                if address == 255 {
                    if part == 1 {
                        return y // For part 1, return the first Y sent to 255
                    } else {
                        natPacket = (x, y)
                    }
                } else {
                    messageQueues[address].append((x, y))
                    allIdle = false // A packet was sent, so network is not idle
                }
            }
            if !computer.inputs.isEmpty || !messageQueues[i].isEmpty {
                allIdle = false
            }
            
        }
        
        //NAT Logic for Part 2
        if part == 2 && allIdle, let packet = natPacket {
            if let lastY = lastSentY, lastY == packet.y {
                return packet.y // Return Y if delivered twice in a row by NAT
            }
            messageQueues[0].append(packet)
            lastSentY = packet.y
            natPacket = nil // Consume the NAT packet
        }
    }
}

// MARK: - Main

func main() {
    do {
        // Read input from file
        let input = try String(contentsOfFile: "input.txt", encoding: .utf8)
        let program = input.trimmingCharacters(in: .whitespacesAndNewlines).split(separator: ",").compactMap { Int($0) }

        // Part 1
        let part1Result = simulateNetwork(program: program, part: 1)
        print("Part 1: \(part1Result)")
        
        //Part 2
        let part2Result = simulateNetwork(program: program, part: 2)
        print("Part 2: \(part2Result)")

    } catch {
        print("Error reading file: \(error)")
    }
}

// Ensure the main function is called
main()

