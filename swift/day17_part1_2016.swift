
import Foundation
import CommonCrypto

struct Point {
    var x: Int
    var y: Int
    var path: String
}

func readPasscode(_ filename: String) -> String {
    do {
        let passcode = try String(contentsOfFile: filename)
        return passcode.trimmingCharacters(in: .whitespacesAndNewlines)
    } catch {
        fatalError("Failed to read passcode")
    }
}

func findShortestPath(_ passcode: String) -> String {
    var queue = [Point(x: 0, y: 0, path: "")]
    while !queue.isEmpty {
        let point = queue.removeFirst()
        
        if point.x == 3 && point.y == 3 {
            return point.path
        }
        
        for dir in getOpenDoors(passcode, path: point.path) {
            var nextPoint = Point(x: point.x, y: point.y, path: point.path + dir)
            switch dir {
            case "U":
                nextPoint.y -= 1
            case "D":
                nextPoint.y += 1
            case "L":
                nextPoint.x -= 1
            case "R":
                nextPoint.x += 1
            default:
                break
            }
            
            if nextPoint.x >= 0 && nextPoint.x < 4 && nextPoint.y >= 0 && nextPoint.y < 4 {
                queue.append(nextPoint)
            }
        }
    }
    return "No path found"
}

func getOpenDoors(_ passcode: String, path: String) -> [String] {
    let hash = md5Hash(passcode + path)
    var doors: [String] = []
    if hash[hash.index(hash.startIndex, offsetBy: 0)] >= "b" && hash[hash.index(hash.startIndex, offsetBy: 0)] <= "f" {
        doors.append("U")
    }
    if hash[hash.index(hash.startIndex, offsetBy: 1)] >= "b" && hash[hash.index(hash.startIndex, offsetBy: 1)] <= "f" {
        doors.append("D")
    }
    if hash[hash.index(hash.startIndex, offsetBy: 2)] >= "b" && hash[hash.index(hash.startIndex, offsetBy: 2)] <= "f" {
        doors.append("L")
    }
    if hash[hash.index(hash.startIndex, offsetBy: 3)] >= "b" && hash[hash.index(hash.startIndex, offsetBy: 3)] <= "f" {
        doors.append("R")
    }
    return doors
}

func md5Hash(_ input: String) -> String {
    let data = input.data(using: .utf8)!
    var hash = [UInt8](repeating: 0, count: Int(CC_MD5_DIGEST_LENGTH))
    _ = data.withUnsafeBytes { (ptr: UnsafeRawBufferPointer) in
        CC_MD5(ptr.baseAddress, CC_LONG(data.count), &hash)
    }
    return hash.map { String(format: "%02hhx", $0) }.joined()
}

let passcode = readPasscode("input.txt")
let path = findShortestPath(passcode)
print(path)
