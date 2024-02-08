
import java.math.BigDecimal
import java.math.BigInteger
import java.nio.file.Files
import java.nio.file.Paths

class RatVec3 {
    BigDecimal X
    BigDecimal Y
    BigDecimal Z

    RatVec3(BigDecimal x, BigDecimal y, BigDecimal z) {
        X = x
        Y = y
        Z = z
    }

    RatVec3 Add(RatVec3 other) {
        return new RatVec3(X.add(other.X), Y.add(other.Y), Z.add(other.Z))
    }

    RatVec3 Subtract(RatVec3 other) {
        return new RatVec3(X.subtract(other.X), Y.subtract(other.Y), Z.subtract(other.Z))
    }

    RatVec3 Multiply(BigDecimal s) {
        return new RatVec3(X.multiply(s), Y.multiply(s), Z.multiply(s))
    }

    RatVec3 Divide(BigDecimal s) {
        return new RatVec3(X.divide(s), Y.divide(s), Z.divide(s))
    }

    RatVec3 Cross(RatVec3 other) {
        return new RatVec3(
            Y.multiply(other.Z).subtract(Z.multiply(other.Y)),
            Z.multiply(other.X).subtract(X.multiply(other.Z)),
            X.multiply(other.Y).subtract(Y.multiply(other.X))
        )
    }

    BigDecimal Dot(RatVec3 other) {
        return X.multiply(other.X).add(Y.multiply(other.Y)).add(Z.multiply(other.Z))
    }
}

class HailStone {
    RatVec3 p
    RatVec3 v

    HailStone(RatVec3 p, RatVec3 v) {
        this.p = p
        this.v = v
    }

    HailStone Subtract(HailStone other) {
        return new HailStone(p.Subtract(other.p), v.Subtract(other.v))
    }
}

List<HailStone> readInput(List<String> input) {
    List<HailStone> res = []
    for (String line : input) {
        res.add(readLine(line))
    }
    return res
}

HailStone readLine(String line) {
    def match = line.findAll(/-*\d+/) as List
    def m = match.collect { new BigInteger(it) }

    return new HailStone(
        new RatVec3(new BigDecimal(m[0]), new BigDecimal(m[1]), new BigDecimal(m[2])),
        new RatVec3(new BigDecimal(m[3]), new BigDecimal(m[4]), new BigDecimal(m[5]))
    )
}

BigDecimal intersectionTime(HailStone r, HailStone s) {
    RatVec3 plane = r.p.Cross(r.p.Add(r.v))
    return s.p.Dot(plane).multiply(new BigDecimal(-1)).divide(s.v.Dot(plane))
}

BigDecimal solve(List<String> input) {
    List<HailStone> hailStones = readInput(input[0..2])
    HailStone s1 = hailStones[1]
    HailStone s2 = hailStones[2]
    HailStone ref1 = s1.Subtract(hailStones[0])
    HailStone ref2 = s2.Subtract(hailStones[0])

    BigDecimal t1 = intersectionTime(ref2, ref1)
    BigDecimal t2 = intersectionTime(ref1, ref2)

    RatVec3 rock1 = s1.p.Add(s1.v.Multiply(t1))
    RatVec3 rock2 = s2.p.Add(s2.v.Multiply(t2))

    RatVec3 rp = rock1.Subtract(rock2.Subtract(rock1).Divide(t2.subtract(t1)).Multiply(t1))
    return rp.X.add(rp.Y).add(rp.Z)
}

List<String> input = Files.readAllLines(Paths.get("input.txt"))
println solve(input)
