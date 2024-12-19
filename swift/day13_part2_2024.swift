
import Foundation

struct Machine {
    var ax: Int64, ay: Int64, bx: Int64, by: Int64, px: Int64, py: Int64
}

func readInput(filename: String) -> [Machine] {
    guard let fileURL = URL(string: "file://\(FileManager.default.currentDirectoryPath)/\(filename)"),
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
    var m = Machine(ax: 0, ay: 0, bx: 0, by: 0, px: 0, py: 0)
    for line in lines {
        var l = line
        l = l.replacingOccurrences(of: "Button A:", with: "A:")
        l = l.replacingOccurrences(of: "Button B:", with: "B:")
        l = l.replacingOccurrences(of: "Prize:", with: "P:")
        if l.hasPrefix("A:") {
            (m.ax, m.ay) = parseLine(s: String(l.dropFirst(2)))
        } else if l.hasPrefix("B:") {
            (m.bx, m.by) = parseLine(s: String(l.dropFirst(2)))
        } else if l.hasPrefix("P:") {
            (m.px, m.py) = parsePrize(s: String(l.dropFirst(2)))
        }
    }
    return m
}

func parseLine(s: String) -> (Int64, Int64) {
    let parts = s.trimmingCharacters(in: .whitespaces).components(separatedBy: ",")
    let x = parseVal(s: parts[0])
    let y = parseVal(s: parts[1])
    return (x, y)
}

func parsePrize(s: String) -> (Int64, Int64) {
    let parts = s.trimmingCharacters(in: .whitespaces).components(separatedBy: ",")
    let x = parseValPrize(s: parts[0])
    let y = parseValPrize(s: parts[1])
    return (x, y)
}

func parseVal(s: String) -> Int64 {
    var str = s.trimmingCharacters(in: .whitespaces)
    str = str.replacingOccurrences(of: "X+", with: "")
    str = str.replacingOccurrences(of: "Y+", with: "")
    str = str.replacingOccurrences(of: "X=", with: "")
    str = str.replacingOccurrences(of: "Y=", with: "")
    return Int64(str) ?? 0
}

func parseValPrize(s: String) -> Int64 {
    var str = s.trimmingCharacters(in: .whitespaces)
    str = str.replacingOccurrences(of: "X=", with: "")
    str = str.replacingOccurrences(of: "Y=", with: "")
    return Int64(str) ?? 0
}

func solveMachine(m: Machine) -> Int64 {
    let D = m.ax * m.by - m.ay * m.bx
    if D == 0 {
        return -1
    }
    let numA = m.px * m.by - m.py * m.bx
    let numB = -m.px * m.ay + m.py * m.ax
    if numA % D != 0 || numB % D != 0 {
        return -1
    }
    let a = numA / D
    let b = numB / D
    if a < 0 || b < 0 {
        return -1
    }
    return 3 * a + b
}

let offset: Int64 = 10000000000000
var machines = readInput(filename: "input.txt")
for i in 0..<machines.count {
    machines[i].px += offset
    machines[i].py += offset
}

var results: [Int64] = []
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
