
import CryptoKit
import Foundation

func findPassword(doorID: String) -> String {
    var password = ""
    var i = 0
    while password.count < 8 {
        let data = (doorID + String(i)).data(using: .utf8)!
        let digest = Insecure.MD5.hash(data: data)
        let hex = digest.map { String(format: "%02x", $0) }.joined()
        if hex.hasPrefix("00000") {
            password.append(hex[hex.index(hex.startIndex, offsetBy: 5)])
        }
        i += 1
    }
    return password
}

let doorID = try! String(contentsOfFile: "input.txt").trimmingCharacters(in: .whitespacesAndNewlines)
print(findPassword(doorID: doorID))
