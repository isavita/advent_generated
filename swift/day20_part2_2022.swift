
import Foundation

struct Num: Equatable {
    let id: Int
    var val: Int
}

func solve() -> Int {
    let input = try! String(contentsOfFile: "input.txt").trimmingCharacters(in: .whitespacesAndNewlines)
    let original = input.split(separator: "\n").enumerated().map { Num(id: $0.offset, val: Int($0.element)! * 811589153) }
    var mixed = original

    for _ in 0..<10 {
        for num in original {
            let oldIndex = mixed.firstIndex(of: num)!
            mixed.remove(at: oldIndex)
            let newIndex = (oldIndex + num.val) % mixed.count
            mixed.insert(num, at: newIndex >= 0 ? newIndex : newIndex + mixed.count)
        }
    }

    let zeroIndex = mixed.firstIndex(where: { $0.val == 0 })!
    return [1000, 2000, 3000].reduce(0) { $0 + mixed[($1 + zeroIndex) % mixed.count].val }
}

print(solve())
