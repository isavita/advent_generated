import Foundation

func isRealRoom(_ room: String) -> Bool {
    let parts = room.components(separatedBy: "[")
    let checksum = String(parts[1].dropLast())
    let encryptedName = parts[0].components(separatedBy: "-").dropLast()
    
    var letterCounts: [Character: Int] = [:]
    for part in encryptedName {
        for letter in part {
            letterCounts[letter, default: 0] += 1
        }
    }
    
    var counts: [(Character, Int)] = Array(letterCounts).sorted { a, b in
        if a.1 == b.1 {
            return a.0 < b.0
        }
        return a.1 > b.1
    }
    
    for i in 0..<checksum.count {
        if checksum[checksum.index(checksum.startIndex, offsetBy: i)] != counts[i].0 {
            return false
        }
    }
    
    return true
}

func getSectorID(_ room: String) -> Int {
    let parts = room.components(separatedBy: "-")
    let sectorIDPart = parts.last!
    let sectorID = Int(sectorIDPart.components(separatedBy: "[").first!)!
    return sectorID
}

func decryptName(_ room: String) -> String {
    let parts = room.components(separatedBy: "-")
    let sectorIDPart = parts.last!
    let sectorID = Int(sectorIDPart.components(separatedBy: "[").first!)!
    var decryptedName = ""
    
    for part in parts.dropLast() {
        for letter in part {
            if letter == "-" {
                decryptedName += " "
            } else {
                let shiftedAscii = Int(letter.asciiValue!) - 97 + sectorID
                let shiftedAsciiModulo = shiftedAscii % 26
                let shiftedLetter = Character(UnicodeScalar(shiftedAsciiModulo + 97)!)
                decryptedName += String(shiftedLetter)
            }
        }
        decryptedName += " "
    }
    
    return decryptedName.trimmingCharacters(in: .whitespaces)
}

func main() {
    do {
        let fileURL = URL(fileURLWithPath: "input.txt")
        let content = try String(contentsOf: fileURL, encoding: .utf8)
        let lines = content.components(separatedBy: "\n")
        
        for line in lines {
            if isRealRoom(line) {
                let decryptedName = decryptName(line)
                if decryptedName.contains("northpole object") {
                    print(getSectorID(line))
                    break
                }
            }
        }
    } catch {
        print("Error reading file")
    }
}

main()