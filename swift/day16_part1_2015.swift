
import Foundation

let mfcsam = ["children": 3, "cats": 7, "samoyeds": 2, "pomeranians": 3, "akitas": 0, "vizslas": 0, "goldfish": 5, "trees": 3, "cars": 2, "perfumes": 1]
let fileURL = URL(fileURLWithPath: "input.txt")
let content = try String(contentsOf: fileURL)
let lines = content.split(separator: "\n")

for line in lines {
    let parts = line.split(separator: " ")
    let sueNumber = parts[1].dropLast()
    var matches = true
    var compounds = [String: Int]()
    
    for i in stride(from: 2, to: parts.count, by: 2) {
        let item = String(parts[i].dropLast())
        let count = Int(parts[i+1].dropLast( i + 1 < parts.count && parts[i+1].last == "," ? 1 : 0))!
        compounds[item] = count
    }
    
    for (item, count) in compounds {
        if mfcsam[item] != count {
            matches = false
            break
        }
    }

    if matches {
        print(sueNumber)
        break
    }
}
