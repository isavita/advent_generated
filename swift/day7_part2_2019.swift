
import Foundation

// Function to read the Intcode program from a file
func readIntcodeProgram(fromFile filePath: String) -> [Int] {
    guard let fileContents = try? String(contentsOfFile: filePath) else {
        fatalError("Could not read file: \(filePath)")
    }
    return fileContents.trimmingCharacters(in: .whitespacesAndNewlines)
                       .components(separatedBy: ",")
                       .compactMap { Int($0) }
}

// Intcode computer implementation
class IntcodeComputer {
    var memory: [Int]
    var ip: Int = 0
    var inputs: [Int] = []
    var outputs: [Int] = []
    var isHalted: Bool = false

    init(program: [Int]) {
        self.memory = program
    }

    func run() {
        while ip < memory.count {
            let opcode = memory[ip] % 100
            let modes = String(memory[ip] / 100).reversed().map { Int(String($0)) ?? 0 }

            switch opcode {
            case 1: // Add
                let param1 = getValue(at: ip + 1, mode: modes.count > 0 ? modes[0] : 0)
                let param2 = getValue(at: ip + 2, mode: modes.count > 1 ? modes[1] : 0)
                let resultAddress = memory[ip + 3]
                memory[resultAddress] = param1 + param2
                ip += 4
            case 2: // Multiply
                let param1 = getValue(at: ip + 1, mode: modes.count > 0 ? modes[0] : 0)
                let param2 = getValue(at: ip + 2, mode: modes.count > 1 ? modes[1] : 0)
                let resultAddress = memory[ip + 3]
                memory[resultAddress] = param1 * param2
                ip += 4
            case 3: // Input
                if inputs.isEmpty { return } // Wait for input
                let input = inputs.removeFirst()
                let address = memory[ip + 1]
                memory[address] = input
                ip += 2
            case 4: // Output
                let output = getValue(at: ip + 1, mode: modes.count > 0 ? modes[0] : 0)
                outputs.append(output)
                ip += 2
            case 5: // Jump-if-true
                let param1 = getValue(at: ip + 1, mode: modes.count > 0 ? modes[0] : 0)
                let param2 = getValue(at: ip + 2, mode: modes.count > 1 ? modes[1] : 0)
                ip = param1 != 0 ? param2 : ip + 3
            case 6: // Jump-if-false
                let param1 = getValue(at: ip + 1, mode: modes.count > 0 ? modes[0] : 0)
                let param2 = getValue(at: ip + 2, mode: modes.count > 1 ? modes[1] : 0)
                ip = param1 == 0 ? param2 : ip + 3
            case 7: // Less than
                let param1 = getValue(at: ip + 1, mode: modes.count > 0 ? modes[0] : 0)
                let param2 = getValue(at: ip + 2, mode: modes.count > 1 ? modes[1] : 0)
                let resultAddress = memory[ip + 3]
                memory[resultAddress] = param1 < param2 ? 1 : 0
                ip += 4
            case 8: // Equals
                let param1 = getValue(at: ip + 1, mode: modes.count > 0 ? modes[0] : 0)
                let param2 = getValue(at: ip + 2, mode: modes.count > 1 ? modes[1] : 0)
                let resultAddress = memory[ip + 3]
                memory[resultAddress] = param1 == param2 ? 1 : 0
                ip += 4
            case 99: // Halt
                isHalted = true
                return
            default:
                fatalError("Invalid opcode: \(opcode)")
            }
        }
    }

    private func getValue(at address: Int, mode: Int) -> Int {
        switch mode {
        case 0: // Position mode
            return memory[memory[address]]
        case 1: // Immediate mode
            return memory[address]
        default:
            fatalError("Invalid parameter mode: \(mode)")
        }
    }
}

// Function to generate permutations of an array
func permutations<T>(of array: [T]) -> [[T]] {
    guard array.count > 1 else { return [array] }
    var result: [[T]] = []
    for (index, element) in array.enumerated() {
        var subArray = array
        subArray.remove(at: index)
        let subPermutations = permutations(of: subArray)
        for var subPermutation in subPermutations {
            subPermutation.insert(element, at: 0)
            result.append(subPermutation)
        }
    }
    return result
}

// Function to run the amplifier circuit
func runAmplifierCircuit(program: [Int], phaseSettings: [Int]) -> Int {
    var signal = 0
    for phaseSetting in phaseSettings {
        let computer = IntcodeComputer(program: program)
        computer.inputs = [phaseSetting, signal]
        computer.run()
        signal = computer.outputs.last!
    }
    return signal
}

// Function to run the amplifier circuit in feedback loop mode
func runAmplifierFeedbackLoop(program: [Int], phaseSettings: [Int]) -> Int {
    let numAmplifiers = phaseSettings.count
    var amplifiers: [IntcodeComputer] = []
    for _ in 0..<numAmplifiers {
        amplifiers.append(IntcodeComputer(program: program))
    }

    // Initialize phase settings
    for i in 0..<numAmplifiers {
        amplifiers[i].inputs.append(phaseSettings[i])
    }

    var signal = 0
    var amplifierIndex = 0
    var lastOutput = 0

    while !amplifiers.last!.isHalted {
        amplifiers[amplifierIndex].inputs.append(signal)
        amplifiers[amplifierIndex].run()
        
        if !amplifiers[amplifierIndex].outputs.isEmpty {
            signal = amplifiers[amplifierIndex].outputs.removeFirst()
            if amplifierIndex == numAmplifiers - 1 {
                lastOutput = signal
            }
        }
        
        amplifierIndex = (amplifierIndex + 1) % numAmplifiers
    }

    return lastOutput
}

// Main execution
let filePath = "input.txt"
let program = readIntcodeProgram(fromFile: filePath)

// Part 1
let phaseSettingsPart1 = [0, 1, 2, 3, 4]
let allPhaseSettingsPart1 = permutations(of: phaseSettingsPart1)
let maxSignalPart1 = allPhaseSettingsPart1.map { runAmplifierCircuit(program: program, phaseSettings: $0) }.max()!
print("Part 1 - Max thruster signal: \(maxSignalPart1)")

// Part 2
let phaseSettingsPart2 = [5, 6, 7, 8, 9]
let allPhaseSettingsPart2 = permutations(of: phaseSettingsPart2)
let maxSignalPart2 = allPhaseSettingsPart2.map { runAmplifierFeedbackLoop(program: program, phaseSettings: $0) }.max()!
print("Part 2 - Max thruster signal: \(maxSignalPart2)")
