
import Foundation
import CommonCrypto

extension Data {
    var md5: Data {
        var digest = [UInt8](repeating: 0, count: Int(CC_MD5_DIGEST_LENGTH))
        _ = withUnsafeBytes { (buffer) in
            CC_MD5(buffer.baseAddress, CC_LONG(count), &digest)
        }
        return Data(digest)
    }
}

if let input = try? String(contentsOfFile: "input.txt") {
    let secretKey = input.trimmingCharacters(in: .whitespacesAndNewlines)
    var number = 0
    
    while true {
        let hash = (secretKey + String(number)).data(using: .utf8)!.md5
        let hashString = hash.map { String(format: "%02hhx", $0) }.joined()
        
        if hashString.hasPrefix("00000") {
            print(number)
            break
        }
        
        number += 1
    }
}
