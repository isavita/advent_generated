import Foundation

func supportsTLS(_ ip: String) -> Bool {
    do {
        let insideBrackets = try NSRegularExpression(pattern: "\\[[a-z]+\\]", options: [])
        let matches = insideBrackets.matches(in: ip, options: [], range: NSRange(location: 0, length: ip.utf16.count))

        for match in matches {
            let bracketContent = (ip as NSString).substring(with: match.range)
            if containsABBA(bracketContent) {
                return false
            }
        }

        let modifiedIp = ip.replacingOccurrences(of: "\\[[a-z]+\\]", with: "-", options: .regularExpression)
        return containsABBA(modifiedIp)
    } catch {
        return false
    }
}

func containsABBA(_ s: String) -> Bool {
    let chars = Array(s)
    for i in 0..<chars.count-3 {
        if chars[i] != chars[i+1] && chars[i] == chars[i+3] && chars[i+1] == chars[i+2] {
            return true
        }
    }
    return false
}

do {
    let fileUrl = URL(fileURLWithPath: "input.txt")
    let content = try String(contentsOf: fileUrl, encoding: .utf8)
    let lines = content.components(separatedBy: "\n")
    var tlsCount = 0
    for line in lines {
        if supportsTLS(line) {
            tlsCount += 1
        }
    }
    print(tlsCount)
} catch {
    print("Error reading file")
}