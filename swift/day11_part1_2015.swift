
import Foundation

func main() {
    guard let currentPassword = readInput(from: "input.txt") else {
        print("Error reading input")
        return
    }
    
    let newPassword = findNextPassword(currentPassword)
    print(newPassword)
}

func readInput(from filename: String) -> String? {
    return try? String(contentsOfFile: filename).trimmingCharacters(in: .whitespacesAndNewlines)
}

func findNextPassword(_ password: String) -> String {
    var password = password
    repeat {
        password = incrementPassword(password)
    } while !isValidPassword(password)
    return password
}

func incrementPassword(_ password: String) -> String {
    var runes = Array(password)
    for i in stride(from: runes.count - 1, through: 0, by: -1) {
        runes[i] = runes[i] == "z" ? "a" : Character(UnicodeScalar(runes[i].unicodeScalars.first!.value + 1)!)
        if runes[i] != "a" { break }
    }
    return String(runes)
}

func isValidPassword(_ password: String) -> Bool {
    return hasStraight(password) && !containsInvalidLetters(password) && hasTwoPairs(password)
}

func hasStraight(_ password: String) -> Bool {
    for i in 0..<(password.count - 2) {
        let index = password.index(password.startIndex, offsetBy: i)
        let first = password[index].unicodeScalars.first!.value
        let second = password[password.index(after: index)].unicodeScalars.first!.value
        let third = password[password.index(index, offsetBy: 2)].unicodeScalars.first!.value
        if second == first + 1 && third == first + 2 {
            return true
        }
    }
    return false
}

func containsInvalidLetters(_ password: String) -> Bool {
    return password.contains { $0 == "i" || $0 == "o" || $0 == "l" }
}

func hasTwoPairs(_ password: String) -> Bool {
    var count = 0
    var i = password.startIndex
    while i < password.index(password.endIndex, offsetBy: -1) {
        let nextIndex = password.index(after: i)
        if password[i] == password[nextIndex] {
            count += 1
            i = password.index(nextIndex, offsetBy: 1)
        } else {
            i = nextIndex
        }
    }
    return count >= 2
}

main()
