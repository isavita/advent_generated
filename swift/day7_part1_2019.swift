
import Foundation

// Function to read the Intcode program from a file
func readProgram(from filename: String) -> [Int] {
    let url = URL(fileURLWithPath: filename)
    let content = try! String(contentsOf: url)
    return content.split(separator: ",").compactMap { Int($0) }
}

// Intcode computer implementation
func runIntcode(program: [Int], inputs: [Int]) -> Int {
    var memory = program
    var inputIndex = 0
    var output: Int = 0
    var instructionPointer = 0
    
    while true {
        let instruction = memory[instructionPointer]
        let opcode = instruction % 100
        let modes = [
            (instruction / 100) % 10,
            (instruction / 1000) % 10,
            (instruction / 10000) % 10
        ]
        
        switch opcode {
        case 1: // Addition
            let param1 = getParameter(memory, instructionPointer + 1, modes[0])
            let param2 = getParameter(memory, instructionPointer + 2, modes[1])
            let resultPosition = memory[instructionPointer + 3]
            memory[resultPosition] = param1 + param2
            instructionPointer += 4
            
        case 2: // Multiplication
            let param1 = getParameter(memory, instructionPointer + 1, modes[0])
            let param2 = getParameter(memory, instructionPointer + 2, modes[1])
            let resultPosition = memory[instructionPointer + 3]
            memory[resultPosition] = param1 * param2
            instructionPointer += 4
            
        case 3: // Input
            let inputPosition = memory[instructionPointer + 1]
            memory[inputPosition] = inputs[inputIndex]
            inputIndex += 1
            instructionPointer += 2
            
        case 4: // Output
            let outputValue = getParameter(memory, instructionPointer + 1, modes[0])
            output = outputValue
            instructionPointer += 2
            
        case 5: // Jump-if-true
            let param1 = getParameter(memory, instructionPointer + 1, modes[0])
            let param2 = getParameter(memory, instructionPointer + 2, modes[1])
            if param1 != 0 {
                instructionPointer = param2
            } else {
                instructionPointer += 3
            }
            
        case 6: // Jump-if-false
            let param1 = getParameter(memory, instructionPointer + 1, modes[0])
            let param2 = getParameter(memory, instructionPointer + 2, modes[1])
            if param1 == 0 {
                instructionPointer = param2
            } else {
                instructionPointer += 3
            }
            
        case 7: // Less than
            let param1 = getParameter(memory, instructionPointer + 1, modes[0])
            let param2 = getParameter(memory, instructionPointer + 2, modes[1])
            let resultPosition = memory[instructionPointer + 3]
            memory[resultPosition] = (param1 < param2) ? 1 : 0
            instructionPointer += 4
            
        case 8: // Equals
            let param1 = getParameter(memory, instructionPointer + 1, modes[0])
            let param2 = getParameter(memory, instructionPointer + 2, modes[1])
            let resultPosition = memory[instructionPointer + 3]
            memory[resultPosition] = (param1 == param2) ? 1 : 0
            instructionPointer += 4
            
        case 99: // Halt
            return output
            
        default:
            fatalError("Unknown opcode: \(opcode)")
        }
    }
}

// Function to get the parameter based on mode
func getParameter(_ memory: [Int], _ position: Int, _ mode: Int) -> Int {
    return mode == 0 ? memory[memory[position]] : memory[position]
}

// Function to generate all permutations of the phase settings
func permutations<T>(_ array: [T]) -> [[T]] {
    guard array.count > 0 else { return [[]] }
    return array.flatMap { element in
        permutations(array.filter { $0 as AnyObject !== element as AnyObject }).map { [element] + $0 }
    }
}

// Main function to calculate the maximum output signal
func maxThrusterSignal(program: [Int]) -> Int {
    let phaseSettings = [0, 1, 2, 3, 4]
    let allPermutations = permutations(phaseSettings)
    
    var maxSignal = Int.min
    
    for phaseSetting in allPermutations {
        var inputSignal = 0
        
        for phase in phaseSetting {
            inputSignal = runIntcode(program: program, inputs: [phase, inputSignal])
        }
        
        maxSignal = max(maxSignal, inputSignal)
    }
    
    return maxSignal
}

// Read the program and calculate the maximum output signal
let program = readProgram(from: "input.txt")
let result = maxThrusterSignal(program: program)
print("Max thruster signal: \(result)")
