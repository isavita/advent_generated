import Foundation
import CryptoKit

var salt = ""
var keys = 0
var index = 0

do {
    let fileContent = try String(contentsOfFile: "input.txt", encoding: .utf8)
    salt = fileContent.trimmingCharacters(in: .whitespacesAndNewlines)
} catch {
    print("Error reading file")
    exit(1)
}

while keys < 64 {
    let hash = getMD5Hash(salt + String(index))
    if let triplet = findTriplet(hash) {
        for i in 1...1000 {
            let nextHash = getMD5Hash(salt + String(index+i))
            if nextHash.contains(String(repeating: triplet, count: 5)) {
                keys += 1
                break
            }
        }
    }
    index += 1
}

print(index - 1)

func getMD5Hash(_ input: String) -> String {
    let data = input.data(using: .utf8)!
    let hash = Insecure.MD5.hash(data: data)
    return hash.map { String(format: "%02hhx", $0) }.joined()
}

func findTriplet(_ hash: String) -> String? {
    for i in 0..<hash.count-2 {
        if hash[hash.index(hash.startIndex, offsetBy: i)] == hash[hash.index(hash.startIndex, offsetBy: i+1)] && hash[hash.index(hash.startIndex, offsetBy: i)] == hash[hash.index(hash.startIndex, offsetBy: i+2)] {
            return String(hash[hash.index(hash.startIndex, offsetBy: i)])
        }
    }
    return nil
}