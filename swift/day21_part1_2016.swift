
import Foundation

func main() {
    guard let input = try? String(contentsOfFile: "input.txt") else { return }
    let operations = input.split(separator: "\n").map { String($0) }
    var password = "abcdefgh"

    for op in operations {
        password = applyOperation(op, password)
    }

    print(password)
}

func applyOperation(_ op: String, _ password: String) -> String {
    let fields = op.split(separator: " ").map { String($0) }
    switch fields[0] {
    case "swap":
        if fields[1] == "position" {
            let x = Int(fields[2])!, y = Int(fields[5])!
            return swapPosition(password, x, y)
        } else {
            let x = fields[2].first!, y = fields[5].first!
            return swapLetter(password, x, y)
        }
    case "rotate":
        if fields[1] == "left" {
            let steps = Int(fields[2])!
            return rotateLeft(password, steps)
        } else if fields[1] == "right" {
            let steps = Int(fields[2])!
            return rotateRight(password, steps)
        } else {
            let x = fields[6].first!
            return rotateBasedOnPosition(password, x)
        }
    case "reverse":
        let x = Int(fields[2])!, y = Int(fields[4])!
        return reversePositions(password, x, y)
    case "move":
        let x = Int(fields[2])!, y = Int(fields[5])!
        return movePosition(password, x, y)
    default:
        return password
    }
}

func swapPosition(_ password: String, _ x: Int, _ y: Int) -> String {
    var chars = Array(password)
    chars.swapAt(x, y)
    return String(chars)
}

func swapLetter(_ password: String, _ x: Character, _ y: Character) -> String {
    return password.map { $0 == x ? y : $0 == y ? x : $0 }.reduce("") { $0 + String($1) }
}

func rotateLeft(_ password: String, _ steps: Int) -> String {
    let steps = steps % password.count
    return String(password.dropFirst(steps)) + String(password.prefix(steps))
}

func rotateRight(_ password: String, _ steps: Int) -> String {
    let steps = steps % password.count
    return String(password.suffix(steps)) + String(password.prefix(password.count - steps))
}

func rotateBasedOnPosition(_ password: String, _ x: Character) -> String {
    let index = password.firstIndex(of: x)!.utf16Offset(in: password)
    let steps = 1 + index + (index >= 4 ? 1 : 0)
    return rotateRight(password, steps)
}

func reversePositions(_ password: String, _ x: Int, _ y: Int) -> String {
    var chars = Array(password)
    let range = x...y
    let reversed = chars[range].reversed()
    for (i, char) in reversed.enumerated() {
        chars[x + i] = char
    }
    return String(chars)
}

func movePosition(_ password: String, _ x: Int, _ y: Int) -> String {
    var chars = Array(password)
    let char = chars.remove(at: x)
    chars.insert(char, at: y)
    return String(chars)
}

main()
