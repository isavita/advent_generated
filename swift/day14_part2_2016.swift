import Foundation
import CryptoKit

let hexBytes: [[UInt8]] = (0..<256).map {
    let hex = String(format: "%02x", $0)
    return Array(hex.utf8)
}

let salt = try! String(contentsOfFile: "input.txt", encoding: .utf8).trimmingCharacters(in: .whitespacesAndNewlines)

var hashCache: [Int: String] = [:]

func computeMD5(data: Data) -> Data {
    Data(Insecure.MD5.hash(data: data))
}

func getStretchedMD5Hash(for index: Int) -> String {
    if let cached = hashCache[index] {
        return cached
    }
    
    let inputString = "\(salt)\(index)"
    let inputData = Data(inputString.utf8)
    var digest = computeMD5(data: inputData)
    
    for _ in 0..<2016 {
        let hexData = Data(digest.flatMap { hexBytes[Int($0)] })
        digest = computeMD5(data: hexData)
    }
    
    let hexString = String(bytes: digest.flatMap { hexBytes[Int($0)] }, encoding: .utf8)!
    hashCache[index] = hexString
    return hexString
}

func findTriplet(in hash: String) -> String {
    let chars = Array(hash)
    for i in 0..<chars.count - 2 {
        if chars[i] == chars[i+1] && chars[i] == chars[i+2] {
            return String(chars[i])
        }
    }
    return ""
}

var keysFound = 0
var index = 0

while keysFound < 64 {
    let hash = getStretchedMD5Hash(for: index)
    let triplet = findTriplet(in: hash)
    if !triplet.isEmpty {
        for i in 1...1000 {
            let nextIndex = index + i
            let nextHash = getStretchedMD5Hash(for: nextIndex)
            if nextHash.contains(String(repeating: triplet, count: 5)) {
                keysFound += 1
                if keysFound == 64 {
                    print(index)
                    exit(0)
                }
                break
            }
        }
    }
    index += 1
}
