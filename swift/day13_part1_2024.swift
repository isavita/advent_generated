
import Foundation

struct Machine {
    let ax, ay, bx, by, px, py: Int
}

func readInput(filename: String) -> [Machine] {
    guard let fileURL = URL(string: "file://" + FileManager.default.currentDirectoryPath + "/" + filename),
          let fileContents = try? String(contentsOf: fileURL) else {
        return []
    }
    
    var machines: [Machine] = []
    var lines: [String] = []
    
    for line in fileContents.components(separatedBy: .newlines) {
        let trimmedLine = line.trimmingCharacters(in: .whitespacesAndNewlines)
        if trimmedLine.isEmpty {
            if !lines.isEmpty {
                machines.append(parseMachine(lines: lines))
                lines = []
            }
        } else {
            lines.append(trimmedLine)
        }
    }
    if !lines.isEmpty {
        machines.append(parseMachine(lines: lines))
    }
    return machines
}

func parseMachine(lines: [String]) -> Machine {
    var ax = 0, ay = 0, bx = 0, by = 0, px = 0, py = 0
    for line in lines {
        var l = line
        l = l.replacingOccurrences(of: "Button A:", with: "A:")
        l = l.replacingOccurrences(of: "Button B:", with: "B:")
        l = l.replacingOccurrences(of: "Prize:", with: "P:")
        if l.hasPrefix("A:") {
            (ax, ay) = parseLine(s: String(l.dropFirst(2)))
        } else if l.hasPrefix("B:") {
            (bx, by) = parseLine(s: String(l.dropFirst(2)))
        } else if l.hasPrefix("P:") {
            (px, py) = parsePrize(s: String(l.dropFirst(2)))
        }
    }
    return Machine(ax: ax, ay: ay, bx: bx, by: by, px: px, py: py)
}

func parseLine(s: String) -> (Int, Int) {
    let parts = s.trimmingCharacters(in: .whitespaces).components(separatedBy: ",")
    let xp = parts[0].trimmingCharacters(in: .whitespaces)
    let yp = parts[1].trimmingCharacters(in: .whitespaces)
    return (parseVal(s: xp), parseVal(s: yp))
}

func parsePrize(s: String) -> (Int, Int) {
    let parts = s.trimmingCharacters(in: .whitespaces).components(separatedBy: ",")
    let xp = parts[0].trimmingCharacters(in: .whitespaces)
    let yp = parts[1].trimmingCharacters(in: .whitespaces)
    return (parseValPrize(s: xp), parseValPrize(s: yp))
}

func parseVal(s: String) -> Int {
    var str = s.trimmingCharacters(in: .whitespaces)
    str = str.replacingOccurrences(of: "X+", with: "")
    str = str.replacingOccurrences(of: "Y+", with: "")
    str = str.replacingOccurrences(of: "X=", with: "")
    str = str.replacingOccurrences(of: "Y=", with: "")
    return Int(str) ?? 0
}

func parseValPrize(s: String) -> Int {
    var str = s.trimmingCharacters(in: .whitespaces)
    str = str.replacingOccurrences(of: "X=", with: "")
    str = str.replacingOccurrences(of: "Y=", with: "")
    return Int(str) ?? 0
}

func solveMachine(m: Machine) -> Int {
    var minCost = -1
    for aCount in 0...100 {
        for bCount in 0...100 {
            let x = m.ax * aCount + m.bx * bCount
            let y = m.ay * aCount + m.by * bCount
            if x == m.px && y == m.py {
                let cost = aCount * 3 + bCount
                if minCost == -1 || cost < minCost {
                    minCost = cost
                }
            }
        }
    }
    return minCost
}

let machines = readInput(filename: "input.txt")
var results: [Int] = []

for m in machines {
    let cost = solveMachine(m: m)
    if cost >= 0 {
        results.append(cost)
    }
}

if results.isEmpty {
    print("0 0")
} else {
    let count = results.count
    let sum = results.reduce(0, +)
    print("\(count) \(sum)")
}
