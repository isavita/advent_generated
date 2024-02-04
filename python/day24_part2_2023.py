
import os
import re
from fractions import Fraction

zero = Fraction(0, 1)

class RatVec3:
    def __init__(self, x, y, z):
        self.X = x
        self.Y = y
        self.Z = z

    def Add(self, other):
        return RatVec3(self.X + other.X, self.Y + other.Y, self.Z + other.Z)

    def Subtract(self, other):
        return RatVec3(self.X - other.X, self.Y - other.Y, self.Z - other.Z)

    def Multiply(self, s):
        return RatVec3(self.X * s, self.Y * s, self.Z * s)

    def Divide(self, s):
        return RatVec3(self.X / s, self.Y / s, self.Z / s)

    def Cross(self, other):
        return RatVec3(self.Y * other.Z - self.Z * other.Y,
                       self.Z * other.X - self.X * other.Z,
                       self.X * other.Y - self.Y * other.X)

    def Dot(self, other):
        return self.X * other.X + self.Y * other.Y + self.Z * other.Z

class HailStone:
    def __init__(self, p, v):
        self.p = p
        self.v = v

    def Subtract(self, other):
        return HailStone(self.p.Subtract(other.p), self.v.Subtract(other.v))

def readFile(fileName):
    with open(fileName, 'r') as file:
        return file.read().splitlines()

def solve(input):
    hailStones = readInput(input[:3])
    s1 = hailStones[1]
    s2 = hailStones[2]
    ref1 = s1.Subtract(hailStones[0])
    ref2 = s2.Subtract(hailStones[0])

    t1 = intersectionTime(ref2, ref1)
    t2 = intersectionTime(ref1, ref2)

    rock1 = s1.p.Add(s1.v.Multiply(t1))
    rock2 = s2.p.Add(s2.v.Multiply(t2))

    rp = rock1.Subtract(rock2.Subtract(rock1).Divide(t2 - t1).Multiply(t1))
    return str(int(rp.X + rp.Y + rp.Z))

def readInput(input):
    res = []
    for line in input:
        res.append(readLine(line))
    return res

def readLine(line):
    m = list(map(int, re.findall(r'-?\d+', line)))
    return HailStone(RatVec3(m[0], m[1], m[2]), RatVec3(m[3], m[4], m[5]))

def countIntersections(stones, start, end):
    count = 0
    for i in range(len(stones)):
        for j in range(len(stones[i+1:])):
            x, y, t1, t2, ok = doMatch2D(stones[i], stones[i+j+1])
            if ok and t1 > zero and t2 >= zero and x >= start and y >= start and x <= end and y <= end:
                count += 1
    return count

def doMatch2D(a, b):
    m1 = a.v.Y / a.v.X
    m2 = b.v.Y / b.v.X
    if m1 == m2:
        return None, None, None, None, False
    x = (m1 * a.p.X - a.p.Y - m2 * b.p.X + b.p.Y) / (m1 - m2)
    y = m1 * (x - a.p.X) + a.p.Y
    t1 = (x - a.p.X) / a.v.X
    t2 = (x - b.p.X) / b.v.X
    return x, y, t1, t2, True

def intersectionTime(r, s):
    plane = r.p.Cross(r.p.Add(r.v))
    return -s.p.Dot(plane) / s.v.Dot(plane)

def add(a, b):
    return a + b

def sub(a, b):
    return a - b

def mul(a, b):
    return a * b

def quo(a, b):
    return a / b

def toInt(s):
    return int(s)

def r(rat):
    return Fraction(rat)

def main():
    input = readFile("input.txt")
    print(solve(input))

if __name__ == "__main__":
    main()
