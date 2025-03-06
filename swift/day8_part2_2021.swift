
import Foundation

func main() {
    do {
        let data = try String(contentsOfFile: "input.txt", encoding: .utf8)
        let lines = data.components(separatedBy: .newlines).filter { !$0.isEmpty }

        var part1Count = 0
        var part2Sum = 0

        for line in lines {
            let parts = line.components(separatedBy: " | ")
            let signalPatterns = parts[0].components(separatedBy: " ")
            let outputValues = parts[1].components(separatedBy: " ")

            // Part 1: Count 1, 4, 7, 8 in output
            for output in outputValues {
                let count = output.count
                if count == 2 || count == 4 || count == 3 || count == 7 {
                    part1Count += 1
                }
            }
            
            // Part 2: Decode and sum output values.
            part2Sum += decodeOutput(signalPatterns: signalPatterns, outputValues: outputValues)
        }

        print("Part 1: \(part1Count)")
        print("Part 2: \(part2Sum)")

    } catch {
        print("Error reading file: \(error)")
    }
}

func decodeOutput(signalPatterns: [String], outputValues: [String]) -> Int {
    var mapping: [Int: Set<Character>] = [:]
    var signalSets: [Set<Character>] = signalPatterns.map { Set($0) }
    
    // Find 1, 4, 7, and 8 based on unique segment counts.
    mapping[1] = signalSets.first { $0.count == 2 }!
    mapping[4] = signalSets.first { $0.count == 4 }!
    mapping[7] = signalSets.first { $0.count == 3 }!
    mapping[8] = signalSets.first { $0.count == 7 }!
    
    // Deduce the rest.  Logic from the problem constraints.
    mapping[9] = signalSets.first { $0.count == 6 && $0.isSuperset(of: mapping[4]!) }!
    mapping[0] = signalSets.first { $0.count == 6 && $0.isSuperset(of: mapping[1]!) && $0 != mapping[9] }!
    mapping[6] = signalSets.first { $0.count == 6 && $0 != mapping[9] && $0 != mapping[0] }!
    mapping[3] = signalSets.first { $0.count == 5 && $0.isSuperset(of: mapping[1]!) }!
    mapping[5] = signalSets.first { $0.count == 5 && mapping[6]!.isSuperset(of: $0) }!
    mapping[2] = signalSets.first { $0.count == 5 && $0 != mapping[3] && $0 != mapping[5] }!
    
    
    var outputNumber = 0
    for outputValue in outputValues {
        let outputSet = Set(outputValue)
        for (digit, segments) in mapping {
            if outputSet == segments {
                outputNumber = outputNumber * 10 + digit
                break
            }
        }
    }
    return outputNumber
}

main()
