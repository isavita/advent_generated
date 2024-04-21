import Foundation

struct Instruction {
    let left: String
    let right: String
}

let ElemToMatch = "ZZZ"

do {
    let fileContent = try String(contentsOfFile: "input.txt", encoding: .utf8)
    let lines = fileContent.components(separatedBy: "\n")
    
    var desertMap: [String: Instruction] = [:]
    
    for line in lines[2...] {
        if line.isEmpty { continue }
        let matches = line.matches(for: "[A-Z]{3}")
        desertMap[matches[0]] = Instruction(left: matches[1], right: matches[2])
    }
    
    var current = "AAA"
    var steps = 0
    
    while current != ElemToMatch {
        for direction in lines[0] {
            if direction == "R" {
                current = desertMap[current]?.right ?? ""
            } else if direction == "L" {
                current = desertMap[current]?.left ?? ""
            }
            steps += 1
            if current == ElemToMatch { break }
        }
    }
    
    print(steps)
} catch {
    print("Error: \(error)")
}

extension String {
    func matches(for regex: String) -> [String] {
        do {
            let regex = try NSRegularExpression(pattern: regex)
            let nsString = self as NSString
            let results  = regex.matches(in: self, range: NSRange(location: 0, length: nsString.length))
            return results.map { nsString.substring(with: $0.range) }
        } catch let error {
            print("invalid regex: \(error.localizedDescription)")
            return []
        }
    }
}