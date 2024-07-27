
import Foundation

func findEntriesThatSumTo2020(in numbers: [Int]) -> Int? {
    var seenNumbers = Set<Int>()
    
    for number in numbers {
        let complement = 2020 - number
        if seenNumbers.contains(complement) {
            return number * complement
        }
        seenNumbers.insert(number)
    }
    return nil
}

func readInput(from fileName: String) -> [Int]? {
    do {
        let contents = try String(contentsOfFile: fileName)
        let numbers = contents.split(separator: "\n").compactMap { Int($0) }
        return numbers
    } catch {
        print("Error reading file: \(error)")
        return nil
    }
}

if let numbers = readInput(from: "input.txt") {
    if let result = findEntriesThatSumTo2020(in: numbers) {
        print("The product of the two entries that sum to 2020 is: \(result)")
    } else {
        print("No two entries sum to 2020.")
    }
} else {
    print("Failed to read numbers from input file.")
}
