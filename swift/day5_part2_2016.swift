import Foundation
import CommonCrypto

let hexChars: [Character] = ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "a", "b", "c", "d", "e", "f"]

func computeHash(doorIDData: Data, index: Int) -> [UInt8]? {
    let indexString = String(index)
    guard let indexData = indexString.data(using: .utf8) else { return nil }
    
    var input = Data(doorIDData)
    input.append(indexData)
    
    var digest = [UInt8](repeating: 0, count: Int(CC_MD5_DIGEST_LENGTH))
    _ = input.withUnsafeBytes { ptr in
        CC_MD5(ptr.baseAddress, CC_LONG(input.count), &digest)
    }
    
    if digest[0] == 0 && digest[1] == 0 && digest[2] & 0xF0 == 0 {
        return digest
    }
    return nil
}

// Updated line with explicit encoding parameter
let doorID = try! String(contentsOfFile: "input.txt", encoding: .utf8)
    .trimmingCharacters(in: .whitespacesAndNewlines)
let doorIDData = doorID.data(using: .utf8)!

var password = Array(repeating: Character(" "), count: 8)
var filledPositions = 0
var index = 0

while filledPositions < 8 {
    if let digest = computeHash(doorIDData: doorIDData, index: index) {
        let pos = Int(digest[2] & 0x0F)
        guard pos >= 0 && pos < 8 && password[pos] == " " else {
            index += 1
            continue
        }
        let charIndex = Int(digest[3] >> 4)
        password[pos] = hexChars[charIndex]
        filledPositions += 1
    }
    index += 1
}

print(String(password))
