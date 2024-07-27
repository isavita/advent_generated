
import Foundation

func readInput(from filename: String) -> String? {
    let fileURL = URL(fileURLWithPath: filename)
    do {
        return try String(contentsOf: fileURL, encoding: .utf8)
    } catch {
        print("Error reading file: \(error)")
        return nil
    }
}

func countCommonYesAnswers(in groups: [String]) -> Int {
    var totalCount = 0
    
    for group in groups {
        let individuals = group.split(separator: "\n").map(String.init)
        let firstPersonAnswers = Set(individuals[0])
        
        let commonAnswers = individuals.dropFirst().reduce(firstPersonAnswers) { (currentCommon, person) in
            return currentCommon.intersection(Set(person))
        }
        
        totalCount += commonAnswers.count
    }
    
    return totalCount
}

func main() {
    guard let input = readInput(from: "input.txt") else {
        return
    }
    
    let groups = input.split(separator: "\n\n").map(String.init)
    let result = countCommonYesAnswers(in: groups)
    
    print("Total count of questions to which everyone answered 'yes': \(result)")
}

main()
