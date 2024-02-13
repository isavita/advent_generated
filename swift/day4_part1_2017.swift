
import Foundation

let fileURL = URL(fileURLWithPath: "input.txt")
let input = try String(contentsOf: fileURL)
let passphrases = input.components(separatedBy: .newlines)

let validPassphrases = passphrases.filter { passphrase in
    let words = passphrase.components(separatedBy: " ")
    return Set(words).count == words.count
}

print(validPassphrases.count)
