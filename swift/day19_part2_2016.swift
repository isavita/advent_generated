
import Foundation

class LLNode {
    var elfNum: Int
    var presents: Int
    var next: LLNode?
    
    init(elfNum: Int, presents: Int) {
        self.elfNum = elfNum
        self.presents = presents
    }
}

func elephant(_ input: String) -> Int {
    guard let startingElves = Int(input) else { return 0 }
    
    var root: LLNode? = LLNode(elfNum: 1, presents: 1)
    var iter = root
    for i in 2...startingElves {
        iter?.next = LLNode(elfNum: i, presents: 1)
        iter = iter?.next
    }
    iter?.next = root
    
    var isOddLength = startingElves % 2 == 1
    var beforeAcross = root
    for _ in 0..<(startingElves/2 - 1) {
        beforeAcross = beforeAcross?.next
    }
    
    while root?.next !== root {
        root?.presents += beforeAcross?.next?.presents ?? 0
        
        beforeAcross?.next = beforeAcross?.next?.next
        
        if isOddLength {
            beforeAcross = beforeAcross?.next
        }
        isOddLength = !isOddLength
        root = root?.next
    }
    
    return root?.elfNum ?? 0
}

let path = URL(fileURLWithPath: FileManager.default.currentDirectoryPath).appendingPathComponent("input.txt").path
let input = try String(contentsOfFile: path).trimmingCharacters(in: .whitespacesAndNewlines)
let ans = elephant(input)
print(ans)
