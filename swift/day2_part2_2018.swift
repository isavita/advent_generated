
import Foundation

// Function to calculate the checksum
func calculateChecksum(boxIDs: [String]) -> Int {
    var countTwo = 0
    var countThree = 0

    for id in boxIDs {
        var letterCounts = [Character: Int]()
        
        // Count occurrences of each letter
        for letter in id {
            letterCounts[letter, default: 0] += 1
        }
        
        // Check for exactly two or three occurrences
        if letterCounts.values.contains(2) {
            countTwo += 1
        }
        if letterCounts.values.contains(3) {
            countThree += 1
        }
    }
    
    return countTwo * countThree
}

// Function to find the correct box IDs that differ by exactly one character
func findCloseIDs(boxIDs: [String]) -> String? {
    for i in 0..<boxIDs.count {
        for j in (i + 1)..<boxIDs.count {
            let id1 = boxIDs[i]
            let id2 = boxIDs[j]
            let differingCharacters = zip(id1, id2).filter { $0.0 != $0.1 }
            
            if differingCharacters.count == 1 {
                // Return the common letters by removing the differing character
                return String(id1.filter { id2.contains($0) })
            }
        }
    }
    return nil
}

// Read input from file
func readInput(from fileName: String) -> [String] {
    do {
        let contents = try String(contentsOfFile: fileName)
        return contents.split(separator: "\n").map { String($0) }
    } catch {
        print("Error reading file: \(error)")
        return []
    }
}

// Main execution
let boxIDs = readInput(from: "input.txt")

// Calculate checksum
let checksum = calculateChecksum(boxIDs: boxIDs)
print("Checksum: \(checksum)")

// Find close box IDs
if let commonLetters = findCloseIDs(boxIDs: boxIDs) {
    print("Common letters in the correct box IDs: \(commonLetters)")
} else {
    print("No close box IDs found.")
}
