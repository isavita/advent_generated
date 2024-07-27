
import Foundation
import CommonCrypto

func md5(_ string: String) -> String {
    let data = string.data(using: .utf8)!
    var digest = [UInt8](repeating: 0, count: Int(CC_MD5_DIGEST_LENGTH))
    data.withUnsafeBytes {
        _ = CC_MD5($0.baseAddress, CC_LONG(data.count), &digest)
    }
    return digest.map { String(format: "%02x", $0) }.joined()
}

func isOpenDoor(_ character: Character) -> Bool {
    return "bcdef".contains(character)
}

func findPaths(passcode: String) -> (shortest: String, longest: Int) {
    let directions = [(0, -1, "U"), (0, 1, "D"), (-1, 0, "L"), (1, 0, "R")]
    var queue: [(x: Int, y: Int, path: String)] = [(0, 0, "")]
    var shortestPath: String?
    var longestPathLength = 0
    
    while !queue.isEmpty {
        let (x, y, path) = queue.removeFirst()
        
        if x == 3 && y == 3 {
            if shortestPath == nil || path.count < shortestPath!.count {
                shortestPath = path
            }
            longestPathLength = max(longestPathLength, path.count)
            continue
        }
        
        let hash = md5(passcode + path)
        for (i, (dx, dy, dir)) in directions.enumerated() {
            let newX = x + dx
            let newY = y + dy
            
            if newX >= 0 && newX < 4 && newY >= 0 && newY < 4 {
                if isOpenDoor(hash[hash.index(hash.startIndex, offsetBy: i)]) {
                    queue.append((newX, newY, path + dir))
                }
            }
        }
    }
    
    return (shortest: shortestPath ?? "", longest: longestPathLength)
}

// Read the passcode from input.txt
if let passcode = try? String(contentsOfFile: "input.txt").trimmingCharacters(in: .whitespacesAndNewlines) {
    let result = findPaths(passcode: passcode)
    print("Shortest path: \(result.shortest)")
    print("Longest path length: \(result.longest)")
} else {
    print("Failed to read the passcode from input.txt")
}
