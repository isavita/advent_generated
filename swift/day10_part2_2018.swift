
import Foundation

class Star {
    var x: Int
    var y: Int
    var vX: Int
    var vY: Int
    var next: Star?
    
    init(x: Int, y: Int, vX: Int, vY: Int) {
        self.x = x
        self.y = y
        self.vX = vX
        self.vY = vY
    }
}

func toInt(_ s: String) -> Int {
    return Int(s) ?? 0
}

if let input = try? String(contentsOfFile: "input.txt") {
    let lines = input.components(separatedBy: "\n")
    let head = Star(x: 0, y: 0, vX: 0, vY: 0)
    var tail: Star? = head
    let re = try! NSRegularExpression(pattern: "position=<\\s*(-?\\d+),\\s*(-?\\d+)> velocity=<\\s*(-?\\d+),\\s*(-?\\d+)>")
    
    for line in lines {
        if let match = re.firstMatch(in: line, range: NSRange(line.startIndex..., in: line)) {
            let x = toInt((line as NSString).substring(with: match.range(at: 1)))
            let y = toInt((line as NSString).substring(with: match.range(at: 2)))
            let vX = toInt((line as NSString).substring(with: match.range(at: 3)))
            let vY = toInt((line as NSString).substring(with: match.range(at: 4)))
            let star = Star(x: x, y: y, vX: vX, vY: vY)
            tail?.next = star
            tail = star
        }
    }
    
    var smallestT = 0
    var smallestArea = Int.max
    
    for t in 1..<100000 {
        var maxX = 0
        var maxY = 0
        var minX = 0
        var minY = 0
        
        var temp = head.next
        while let current = temp {
            let x = current.x + current.vX * t
            if maxX < x {
                maxX = x
            } else if minX > x {
                minX = x
            }
            let y = current.y + current.vY * t
            if maxY < y {
                maxY = y
            } else if minY > y {
                minY = y
            }
            temp = current.next
        }
        
        let lenX = maxX - minX + 1
        let lenY = maxY - minY + 1
        let area = lenX + lenY
        
        if smallestArea > area {
            smallestArea = area
            smallestT = t
        }
    }
    
    print(smallestT)
    
    let t = smallestT
    var maxX = 0
    var maxY = 0
    var minX = 0
    var minY = 0
    
    var temp = head.next
    while let current = temp {
        current.x += current.vX * t
        if maxX < current.x {
            maxX = current.x
        } else if minX > current.x {
            minX = current.x
        }
        current.y += current.vY * t
        if maxY < current.y {
            maxY = current.y
        } else if minY > current.y {
            minY = current.y
        }
        temp = current.next
    }
    
    var mapper = [[Bool]](repeating: [Bool](repeating: false, count: maxX - minX + 1), count: maxY - minY + 1)
    
    temp = head.next
    while let current = temp {
        mapper[current.y - minY][current.x - minX] = true
        temp = current.next
    }
    
    for i in 0..<mapper.count {
        for j in 0..<mapper[0].count {
        }
    }
}
