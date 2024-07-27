
import Foundation

// Function to read instructions from a file
func readInstructions(from file: String) -> [String] {
    do {
        let contents = try String(contentsOfFile: file)
        return contents.components(separatedBy: .newlines).filter { !$0.isEmpty }
    } catch {
        print("Error reading file: \(error)")
        return []
    }
}

// Function to execute the instructions and calculate signal strengths and CRT output
func executeInstructions(_ instructions: [String]) -> (Int, String) {
    var x = 1
    var cycle = 0
    var signalStrengths = [Int]()
    var crtOutput = ""

    for instruction in instructions {
        let parts = instruction.split(separator: " ")
        if parts[0] == "noop" {
            cycle += 1
            drawPixel(&crtOutput, cycle, x)
            checkSignalStrength(&signalStrengths, cycle, x)
        } else if parts[0] == "addx" {
            let value = Int(parts[1])!
            for _ in 0..<2 {
                cycle += 1
                drawPixel(&crtOutput, cycle, x)
                checkSignalStrength(&signalStrengths, cycle, x)
            }
            x += value
        }
    }

    let totalSignalStrength = signalStrengths.reduce(0, +)
    return (totalSignalStrength, crtOutput)
}

// Function to check and store signal strength at specific cycles
func checkSignalStrength(_ signalStrengths: inout [Int], _ cycle: Int, _ x: Int) {
    if (cycle == 20 || (cycle - 20) % 40 == 0) {
        signalStrengths.append(cycle * x)
    }
}

// Function to draw a pixel on the CRT output
func drawPixel(_ crtOutput: inout String, _ cycle: Int, _ x: Int) {
    let position = (cycle - 1) % 40
    let spriteRange = (x - 1)...(x + 1)
    crtOutput.append(spriteRange.contains(position) ? "#" : ".")
    if position == 39 { // New line after every 40 pixels
        crtOutput.append("\n")
    }
}

// Main execution
let instructions = readInstructions(from: "input.txt")
let (totalSignalStrength, crtOutput) = executeInstructions(instructions)

print("Total Signal Strength: \(totalSignalStrength)")
print("CRT Output:\n\(crtOutput)")
