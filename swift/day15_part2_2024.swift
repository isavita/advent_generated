
import Foundation

struct Complex: Hashable {
    var real: Int
    var imag: Int

    static func + (lhs: Complex, rhs: Complex) -> Complex {
        return Complex(real: lhs.real + rhs.real, imag: lhs.imag + rhs.imag)
    }
}

func solve(inputStr: String) -> Int {
    let blocks = inputStr.trimmingCharacters(in: .whitespacesAndNewlines).components(separatedBy: "\n\n")
    let lines = blocks[0].components(separatedBy: "\n")
    var m: [Complex: Character] = [:]
    
    for (y, row) in lines.enumerated() {
        for (x, char) in row.enumerated() {
            m[Complex(real: x, imag: y)] = char
        }
    }

    let steps = blocks[1].replacingOccurrences(of: "\n", with: "").map { char -> Complex in
        switch char {
        case "^": return Complex(real: 0, imag: -1)
        case "<": return Complex(real: -1, imag: 0)
        case ">": return Complex(real: 1, imag: 0)
        case "v": return Complex(real: 0, imag: 1)
        default: fatalError()
        }
    }

    var robot = m.first(where: { $0.value == "@" })!.key
    
    for dir in steps {
        if tryToStep(m: &m, pos: robot, dir: dir) {
            robot = robot + dir
        }
    }

     return m.filter { $0.value == "[" || $0.value == "O" }
             .reduce(0) { $0 + $1.key.real + 100 * $1.key.imag }
}

func tryToStep(m: inout [Complex: Character], pos: Complex, dir: Complex) -> Bool {
    let orig = m
    
    switch m[pos] {
    case ".":
        return true
    case "O", "@":
        if tryToStep(m: &m, pos: pos + dir, dir: dir) {
            m[pos + dir] = m[pos]
            m[pos] = "."
            return true
        }
    case "]":
        if tryToStep(m: &m, pos: pos + Complex(real: -1, imag: 0), dir: dir) {
            return true
        }
    case "[":
        if dir == Complex(real: -1, imag: 0) {
            if tryToStep(m: &m, pos: pos + Complex(real: -1, imag: 0), dir: dir) {
                m[pos + Complex(real: -1, imag: 0)] = "["
                m[pos] = "]"
                m[pos + Complex(real: 1, imag: 0)] = "."
                return true
            }
        } else if dir == Complex(real: 1, imag: 0) {
            if tryToStep(m: &m, pos: pos + Complex(real: 2, imag: 0), dir: dir) {
                m[pos] = "."
                m[pos + Complex(real: 1, imag: 0)] = "["
                m[pos + Complex(real: 2, imag: 0)] = "]"
                return true
            }
        } else {
            if tryToStep(m: &m, pos: pos + dir, dir: dir) &&
                tryToStep(m: &m, pos: pos + Complex(real: 1, imag: 0) + dir, dir: dir) {
                m[pos] = "."
                m[pos + Complex(real: 1, imag: 0)] = "."
                m[pos + dir] = "["
                m[pos + dir + Complex(real: 1, imag: 0)] = "]"
                return true
            }
        }
    default:
        break
    }
     m = orig
     return false
}

func scaleUp(inputStr: String) -> String {
    return inputStr.replacingOccurrences(of: "#", with: "##")
                   .replacingOccurrences(of: ".", with: "..")
                   .replacingOccurrences(of: "O", with: "[]")
                   .replacingOccurrences(of: "@", with: "@.")
}

func main() {
    do {
        let inputStr = try String(contentsOfFile: "input.txt")
         print(solve(inputStr: inputStr))
        print(solve(inputStr: scaleUp(inputStr: inputStr)))
    } catch {
        print("Error reading file: \(error)")
    }
}

main()
