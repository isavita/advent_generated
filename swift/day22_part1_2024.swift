import Foundation

func nextSecret(_ s: Int) -> Int {
    var s = s
    var x = s * 64
    s ^= x
    s &= 0xFFFFFF
    x = s / 32
    s ^= x
    s &= 0xFFFFFF
    x = s * 2048
    s ^= x
    s &= 0xFFFFFF
    return s
}

func main() {
    guard let input = try? String(contentsOfFile: "input.txt") else { return }
    let buyers = input.split(separator: "\n").compactMap { Int($0.trimmingCharacters(in: .whitespaces)) }
    var total = 0
    for b in buyers {
        var s = b
        for _ in 0..<2000 {
            s = nextSecret(s)
        }
        total += s
    }
    print(total)
}

main()