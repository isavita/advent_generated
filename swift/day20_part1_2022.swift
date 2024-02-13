
import Foundation

struct Num {
    var pos: Int
    var val: Int
}

func mix(_ nums: inout [Num]) {
    let n = nums.count - 1
    for i in 0..<nums.count {
        let oldpos = nums[i].pos
        let newpos = ((oldpos + nums[i].val) % n + n) % n
        if oldpos < newpos {
            for j in 0..<nums.count {
                if nums[j].pos > oldpos && nums[j].pos <= newpos {
                    nums[j].pos -= 1
                }
            }
        }
        if newpos < oldpos {
            for j in 0..<nums.count {
                if nums[j].pos >= newpos && nums[j].pos < oldpos {
                    nums[j].pos += 1
                }
            }
        }
        nums[i].pos = newpos
    }
}

func coords(_ nums: [Num]) -> Int {
    let l = nums.count
    var zeroPos = 0
    for num in nums {
        if num.val == 0 {
            zeroPos = num.pos
            break
        }
    }
    var sum = 0
    for num in nums {
        if num.pos == (zeroPos + 1000) % l || num.pos == (zeroPos + 2000) % l || num.pos == (zeroPos + 3000) % l {
            sum += num.val
        }
    }
    return sum
}

func readAll(from file: String) -> String {
    do {
        let path = URL(fileURLWithPath: file)
        return try String(contentsOf: path).trimmingCharacters(in: .whitespacesAndNewlines)
    } catch {
        fatalError(error.localizedDescription)
    }
}

func toInt(_ s: String) -> Int {
    guard let n = Int(s) else {
        fatalError("Invalid input")
    }
    return n
}

var nums = [Num]()
let lines = readAll(from: "input.txt").components(separatedBy: "\n")
for (i, n) in lines.enumerated() {
    nums.append(Num(pos: i, val: toInt(n)))
}
var nums2 = nums.map { Num(pos: $0.pos, val: 811589153 * $0.val) }

mix(&nums)
print(coords(nums))
