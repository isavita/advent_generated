
import Foundation

func readInput(filename: String) -> String? {
    do {
        let content = try String(contentsOfFile: filename)
        return content
    } catch {
        return nil
    }
}

func findNextPassword(currentPassword: String) -> String {
    var password = currentPassword
    repeat {
        password = incrementPassword(password: password)
    } while !isValidPassword(password: password)
    return password
}

func incrementPassword(password: String) -> String {
    var chars = Array(password)
    for i in (0..<chars.count).reversed() {
        let asciiValue = chars[i].asciiValue! + 1
        if asciiValue > Character("z").asciiValue! {
            chars[i] = "a"
        } else {
            chars[i] = Character(UnicodeScalar(asciiValue))
            break
        }
    }
    return String(chars)
}

func isValidPassword(password: String) -> Bool {
    return hasStraight(password: password) && !containsInvalidLetters(password: password) && hasTwoPairs(password: password)
}

func hasStraight(password: String) -> Bool {
    let chars = Array(password)
    for i in 0..<(chars.count - 2) {
        if chars[i].asciiValue! + 1 == chars[i + 1].asciiValue! && chars[i].asciiValue! + 2 == chars[i + 2].asciiValue! {
            return true
        }
    }
    return false
}

func containsInvalidLetters(password: String) -> Bool {
    return password.contains("i") || password.contains("o") || password.contains("l")
}

func hasTwoPairs(password: String) -> Bool {
    var count = 0
    var i = 0
    let chars = Array(password)
    while i < chars.count - 1 {
        if chars[i] == chars[i + 1] {
            count += 1
            i += 2
        } else {
            i += 1
        }
    }
    return count >= 2
}

if let currentPassword = readInput(filename: "input.txt") {
    let firstNewPassword = findNextPassword(currentPassword: currentPassword)
    let secondNewPassword = findNextPassword(currentPassword: firstNewPassword)
    print(secondNewPassword)
}
