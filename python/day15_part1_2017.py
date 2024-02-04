
with open("input.txt", "r") as file:
    genAStart = int(file.readline())
    genBStart = int(file.readline())

genAFactor = 16807
genBFactor = 48271
modulus = 2147483647

genA = genAStart
genB = genBStart
matches = 0

for i in range(40000000):
    genA = (genA * genAFactor) % modulus
    genB = (genB * genBFactor) % modulus

    if genA & 0xFFFF == genB & 0xFFFF:
        matches += 1

print(matches)
