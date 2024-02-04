
with open("input.txt", "r") as file:
    lanternFishCounts = [0]*9
    for line in file:
        fishAges = line.strip().split(",")
        for age in fishAges:
            ageCount = int(age)
            lanternFishCounts[ageCount] += 1

    for i in range(256):
        newLanternFish = lanternFishCounts[0]
        for j in range(len(lanternFishCounts)-1):
            lanternFishCounts[j] = lanternFishCounts[j+1]
        lanternFishCounts[6] += newLanternFish
        lanternFishCounts[8] = newLanternFish

    print(sum(lanternFishCounts))

def sum(numbers):
    return sum(numbers)
