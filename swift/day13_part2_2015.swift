import Foundation

func readHappinessValues(from filename: String) -> [String: [String: Int]] {
    var happinessMap: [String: [String: Int]] = [:]
    do {
        let fileContent = try String(contentsOfFile: filename, encoding: .utf8)
        let lines = fileContent.components(separatedBy: "\n")
        for line in lines {
            let parts = line.components(separatedBy: " ")
            guard parts.count >= 11 else { continue }
            let from = parts[0]
            let to = String(parts[10].dropLast())
            var change = Int(parts[3])!
            if parts[2] == "lose" {
                change = -change
            }
            if happinessMap[from] == nil {
                happinessMap[from] = [:]
            }
            happinessMap[from]?[to] = change
        }
    } catch {
        print("Error reading input:", error)
        return [:]
    }
    return happinessMap
}

func addYourself(to happinessMap: inout [String: [String: Int]]) {
    happinessMap["You"] = [:]
    for guest in happinessMap.keys {
        happinessMap[guest]?["You"] = 0
        happinessMap["You"]?[guest] = 0
    }
}

func getGuestList(from happinessMap: [String: [String: Int]]) -> [String] {
    return Array(happinessMap.keys)
}

func calculateOptimalArrangement(for guests: [String], happinessMap: [String: [String: Int]]) -> Int {
    var maxHappiness = 0
    permute(guests, 0, &maxHappiness, happinessMap)
    return maxHappiness
}

func permute(_ arr: [String], _ i: Int, _ maxHappiness: inout Int, _ happinessMap: [String: [String: Int]]) {
    if i > arr.count {
        return
    }
    if i == arr.count {
        let happiness = calculateHappiness(arr, happinessMap)
        if happiness > maxHappiness {
            maxHappiness = happiness
        }
        return
    }
    for j in i..<arr.count {
        var arr = arr
        arr.swapAt(i, j)
        permute(arr, i + 1, &maxHappiness, happinessMap)
        arr.swapAt(i, j)
    }
}

func calculateHappiness(_ arrangement: [String], _ happinessMap: [String: [String: Int]]) -> Int {
    var happiness = 0
    let n = arrangement.count
    for i in 0..<n {
        let left = (i + n - 1) % n
        let right = (i + 1) % n
        happiness += happinessMap[arrangement[i], default: [:]][arrangement[left], default: 0]
        happiness += happinessMap[arrangement[i], default: [:]][arrangement[right], default: 0]
    }
    return happiness
}

var happinessMap = readHappinessValues(from: "input.txt")
addYourself(to: &happinessMap)
let guests = getGuestList(from: happinessMap)
let maxHappiness = calculateOptimalArrangement(for: guests, happinessMap: happinessMap)
print(maxHappiness)