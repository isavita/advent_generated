
import Foundation

struct Mirror {
    var rows: [Int]
    var cols: [Int]
}

func parseInput(input: [String]) -> [Mirror] {
    var mirrors: [Mirror] = []
    var mirrorStr: [String] = []

    for line in input {
        if line == "" {
            mirrors.append(parseMirror(mirrorStr: mirrorStr))
            mirrorStr = []
        } else {
            mirrorStr.append(line)
        }
    }
    mirrors.append(parseMirror(mirrorStr: mirrorStr))

    return mirrors
}

func parseMirror(mirrorStr: [String]) -> Mirror {
    var mirror = Mirror(rows: Array(repeating: 0, count: mirrorStr.count), cols: Array(repeating: 0, count: mirrorStr[0].count))

    for (y, line) in mirrorStr.enumerated() {
        for (x, char) in line.enumerated() {
            mirror.rows[y] <<= 1
            mirror.cols[x] <<= 1
            if char == "#" {
                mirror.rows[y] += 1
                mirror.cols[x] += 1
            }
        }
    }

    return mirror
}

func getMirrorAxis(lines: [Int]) -> Int {
    for i in 1..<lines.count {
        var isMirror = true

        for j in 0..<min(i, lines.count - i) {
            if lines[i - 1 - j] != lines[i + j] {
                isMirror = false
            }
        }

        if isMirror {
            return i
        }
    }

    return 0
}

func getMirrorAxisWithOneSmudge(lines: [Int]) -> Int {
    for i in 1..<lines.count {
        var isMirror = true
        var numSmudges = 0

        for j in 0..<min(i, lines.count - i) {
            if lines[i - 1 - j] != lines[i + j] {
                if numSmudges > 0 {
                    isMirror = false
                } else {
                    let dif = lines[i - 1 - j] ^ lines[i + j]
                    let isOnlyOneSmudge = (dif & (dif - 1)) == 0
                    if isOnlyOneSmudge {
                        numSmudges += 1
                    } else {
                        isMirror = false
                    }
                }
            }
        }

        if isMirror && numSmudges == 1 {
            return i
        }
    }

    return 0
}

func solve(input: [String]) -> Int {
    let mirrors = parseInput(input: input)

    var res = 0
    for mirror in mirrors {
        res += getMirrorAxisWithOneSmudge(lines: mirror.cols)
        res += getMirrorAxisWithOneSmudge(lines: mirror.rows) * 100
    }
    return res
}

func readFile(fileName: String) -> [String] {
    do {
        let fileContents = try String(contentsOfFile: fileName)
        return fileContents.components(separatedBy: "\n")
    } catch {
        fatalError("Failed to read file: \(error)")
    }
}

let input = readFile(fileName: "input.txt")
print(solve(input: input))
