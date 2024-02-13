import Foundation

let fileURL = URL(fileURLWithPath: "input.txt")
let input = try String(contentsOf: fileURL, encoding: .utf8)
let passphrases = input.components(separatedBy: .newlines)

func isValidPassphrase(_ passphrase: String) -> Bool {
    let words = passphrase.components(separatedBy: " ")
    return Set(words).count == words.count
}

func isValidPassphrasePartTwo(_ passphrase: String) -> Bool {
    let words = passphrase.components(separatedBy: " ")
    let sortedWords = words.map { String($0.sorted()) }
    return Set(sortedWords).count == sortedWords.count
}

let validPassphrases = passphrases.filter { isValidPassphrase($0) }
let validPassphrasesPartTwo = passphrases.filter { isValidPassphrasePartTwo($0) }

print(validPassphrases.count)
print(validPassphrasesPartTwo.count)