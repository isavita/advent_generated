
import Foundation

func readInput() -> (Int, Int) {
    // Read the input from the file "input.txt"
    let url = URL(fileURLWithPath: "input.txt")
    let content = try! String(contentsOf: url)
    let lines = content.split(separator: "\n")
    
    // Extract the starting values for generator A and B
    let generatorAStart = Int(lines[0].split(separator: " ").last!)!
    let generatorBStart = Int(lines[1].split(separator: " ").last!)!
    
    return (generatorAStart, generatorBStart)
}

func generateNextValue(current: Int, factor: Int) -> Int {
    return (current * factor) % 2147483647
}

func countMatches(generatorAStart: Int, generatorBStart: Int, iterations: Int) -> Int {
    var count = 0
    var generatorA = generatorAStart
    var generatorB = generatorBStart
    
    for _ in 0..<iterations {
        generatorA = generateNextValue(current: generatorA, factor: 16807)
        generatorB = generateNextValue(current: generatorB, factor: 48271)
        
        // Compare the lowest 16 bits
        if (generatorA & 0xFFFF) == (generatorB & 0xFFFF) {
            count += 1
        }
    }
    
    return count
}

func main() {
    let (generatorAStart, generatorBStart) = readInput()
    let iterations = 40_000_000
    let matchCount = countMatches(generatorAStart: generatorAStart, generatorBStart: generatorBStart, iterations: iterations)
    
    print("Total matches in the lowest 16 bits: \(matchCount)")
}

main()
