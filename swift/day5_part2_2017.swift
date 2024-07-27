
import Foundation

func readInput(from file: String) -> [Int] {
    do {
        let content = try String(contentsOfFile: file)
        return content.split(separator: "\n").compactMap { Int($0) }
    } catch {
        print("Error reading file: \(error)")
        return []
    }
}

func countStepsToExit(instructions: inout [Int]) -> Int {
    var steps = 0
    var currentIndex = 0

    while currentIndex < instructions.count {
        let jump = instructions[currentIndex]
        instructions[currentIndex] += (jump >= 3) ? -1 : 1
        currentIndex += jump
        steps += 1
    }

    return steps
}

func main() {
    let filePath = "input.txt"
    var instructions = readInput(from: filePath)
    
    let stepsToExit = countStepsToExit(instructions: &instructions)
    print("Steps to exit: \(stepsToExit)")
}

main()
