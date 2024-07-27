
import Foundation

// Function to perform the look-and-say transformation
func lookAndSay(_ input: String) -> String {
    var result = ""
    var count = 1
    let characters = Array(input)
    
    for i in 1..<characters.count {
        if characters[i] == characters[i - 1] {
            count += 1
        } else {
            result += "\(count)\(characters[i - 1])"
            count = 1
        }
    }
    // Append the last counted character
    result += "\(count)\(characters.last!)"
    
    return result
}

// Function to generate the look-and-say sequence for a given number of iterations
func generateLookAndSaySequence(initial: String, iterations: Int) -> String {
    var current = initial
    for _ in 0..<iterations {
        current = lookAndSay(current)
    }
    return current
}

// Read input from the file
func readInput(from filename: String) -> String? {
    do {
        let content = try String(contentsOfFile: filename, encoding: .utf8)
        return content.trimmingCharacters(in: .whitespacesAndNewlines)
    } catch {
        print("Error reading file: \(error)")
        return nil
    }
}

// Main execution
if let input = readInput(from: "input.txt") {
    // Part 1: Generate the sequence for 40 iterations
    let resultAfter40 = generateLookAndSaySequence(initial: input, iterations: 40)
    print("Length after 40 iterations: \(resultAfter40.count)")

    // Part 2: Generate the sequence for 50 iterations
    let resultAfter50 = generateLookAndSaySequence(initial: input, iterations: 50)
    print("Length after 50 iterations: \(resultAfter50.count)")
}
