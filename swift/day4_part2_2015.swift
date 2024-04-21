import Foundation
import CryptoKit

func main() {
    do {
        let data = try String(contentsOfFile: "input.txt", encoding: .utf8)
        let secretKey = data.trimmingCharacters(in: .whitespacesAndNewlines)
        var number = 0
        while true {
            let combinedString = "\(secretKey)\(number)"
            let combinedData = combinedString.data(using: .utf8)!
            let hash = Insecure.MD5.hash(data: combinedData)
            let hashString = hash.map { String(format: "%02hhx", $0) }.joined()
            if hashString.hasPrefix("000000") {
                print(number)
                break
            }
            number += 1
        }
    } catch {
        print("Error: \(error)")
    }
}

main()