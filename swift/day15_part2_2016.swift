import Foundation

struct Disc {
    let totalPositions: Int
    let startPosition: Int
}

func main() {
    do {
        let fileContent = try String(contentsOfFile: "input.txt", encoding: .utf8)
        let lines = fileContent.components(separatedBy: "\n")
        var discs: [Disc] = []
        
        for line in lines {
            let regex = try! NSRegularExpression(pattern: "Disc #(\\d+) has (\\d+) positions; at time=0, it is at position (\\d+).")
            let matches = regex.matches(in: line, range: NSRange(location: 0, length: line.utf16.count))
            for match in matches {
                let nsRange = match.range(at: 2)
                let start = line.index(line.startIndex, offsetBy: nsRange.location)
                let end = line.index(start, offsetBy: nsRange.length)
                let totalPositions = Int(line[start..<end])!
                
                let nsRange2 = match.range(at: 3)
                let start2 = line.index(line.startIndex, offsetBy: nsRange2.location)
                let end2 = line.index(start2, offsetBy: nsRange2.length)
                let startPosition = Int(line[start2..<end2])!
                
                discs.append(Disc(totalPositions: totalPositions, startPosition: startPosition))
            }
        }
        
        // Add the new disc as per Part Two's requirement
        discs.append(Disc(totalPositions: 11, startPosition: 0))
        
        var time = 0
        while true {
            if checkDiscs(discs: discs, time: time) {
                print(time)
                break
            }
            time += 1
        }
    } catch {
        print("Error reading file")
    }
}

func checkDiscs(discs: [Disc], time: Int) -> Bool {
    for (i, disc) in discs.enumerated() {
        let position = (disc.startPosition + time + i + 1) % disc.totalPositions
        if position != 0 {
            return false
        }
    }
    return true
}

main()