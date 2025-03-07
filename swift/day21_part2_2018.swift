
import Foundation

func main() {
    var register5: Int = 0
    var seen: Set<Int> = []
    var lastUnique: Int = 0

    while true {
        var register3: Int = register5 | 65536
        register5 = 7586220

        while true {
            let register1: Int = register3 & 255
            register5 = (((register5 + register1) & 16777215) * 65899) & 16777215

            if register3 < 256 {
                if seen.contains(register5) {
                    print("Part Two Answer: \(lastUnique)")
                    return
                }
                seen.insert(register5)
                lastUnique = register5
                break
            } else {
                register3 = register3 / 256
            }
        }
    }
}

main()
