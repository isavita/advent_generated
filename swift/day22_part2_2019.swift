import Foundation

func modMul(_ a: Int64, _ b: Int64, mod: Int64) -> Int64 {
    var a = (a % mod + mod) % mod
    var b = (b % mod + mod) % mod
    var res: Int64 = 0
    while b > 0 {
        if (b & 1) == 1 { res = (res + a) % mod }
        a = (a << 1) % mod
        b >>= 1
    }
    return res
}

func modPow(_ base: Int64, _ exp: Int64, mod: Int64) -> Int64 {
    var result: Int64 = 1
    var base = (base % mod + mod) % mod
    var exp = exp
    while exp > 0 {
        if (exp & 1) == 1 { result = modMul(result, base, mod: mod) }
        base = modMul(base, base, mod: mod)
        exp >>= 1
    }
    return result
}

func modInv(_ a: Int64, mod: Int64) -> Int64 {
    return modPow(a, mod - 2, mod: mod)
}

func main() {
    let size: Int64 = 119315717514047
    let iterations: Int64 = 101741582076661
    guard let input = try? String(contentsOfFile: "input.txt") else { return }
    let lines = input.split(separator: "\n").map { String($0) }
    
    var deckTop: Int64 = 0
    var deckStep: Int64 = 1
    var deckDir: Int64 = 1
    
    for line in lines {
        if line == "deal into new stack" {
            deckTop = (deckTop - deckDir * deckStep + size) % size
            deckDir = -deckDir
        } else if line.hasPrefix("cut") {
            let n = Int64(line.split(separator: " ").last!)!
            deckTop = (deckTop + deckDir * modMul(deckStep, n, mod: size) + size) % size
        } else if line.hasPrefix("deal with increment") {
            let n = Int64(line.split(separator: " ").last!)!
            let inv = modInv(n, mod: size)
            deckStep = modMul(deckStep, inv, mod: size)
            deckTop = modMul(deckTop, inv, mod: size)
        }
    }
    
    var offset: Int64 = 0
    var increment: Int64 = 1
    
    for line in lines {
        if line == "deal into new stack" {
            increment = (-increment).quotientAndRemainder(dividingBy: size).remainder
            if increment < 0 { increment += size }
            offset = (offset + increment) % size
        } else if line.hasPrefix("cut") {
            let n = Int64(line.split(separator: " ").last!)!
            offset = (offset + modMul(increment, n, mod: size)) % size
        } else if line.hasPrefix("deal with increment") {
            let n = Int64(line.split(separator: " ").last!)!
            let inv = modInv(n, mod: size)
            increment = modMul(increment, inv, mod: size)
        }
    }
    
    let finalIncrement = modPow(increment, iterations, mod: size)
    let diff = (finalIncrement - 1 + size) % size
    let inv = modInv((increment - 1 + size) % size, mod: size)
    let finalOffset = modMul(modMul(diff, inv, mod: size), offset, mod: size)
    let answer = (modMul(2020, finalIncrement, mod: size) + finalOffset) % size
    print(answer)
}

main()