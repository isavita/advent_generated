
import Foundation

let input = try String(contentsOfFile: "input.txt").trimmingCharacters(in: .whitespacesAndNewlines)
let keys = input.components(separatedBy: "\n").compactMap { Int($0) }

func transformSubjectNumber(subjectNumber: Int, loopSize: Int) -> Int {
    var value = 1
    for _ in 0..<loopSize {
        value *= subjectNumber
        value %= 20201227
    }
    return value
}

func findLoopSize(publicKey: Int) -> Int {
    var value = 1
    var loopSize = 0
    while value != publicKey {
        loopSize += 1
        value *= 7
        value %= 20201227
    }
    return loopSize
}

let cardPublicKey = keys[0]
let doorPublicKey = keys[1]

let cardLoopSize = findLoopSize(publicKey: cardPublicKey)
let encryptionKey = transformSubjectNumber(subjectNumber: doorPublicKey, loopSize: cardLoopSize)

print(encryptionKey)
