
import Foundation

if let input = try? String(contentsOfFile: "input.txt") {
    let groups = input.components(separatedBy: "\n\n")
    let sum = groups.reduce(0) { result, group in
        let answers = group.replacingOccurrences(of: "\n", with: "")
        let uniqueAnswers = Set(answers)
        return result + uniqueAnswers.count
    }
    print(sum)
}
