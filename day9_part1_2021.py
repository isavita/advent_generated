
with open("input.txt", "r") as file:
    heightmap = [[int(char) for char in line.strip()] for line in file]

totalRiskLevel = 0
for y, row in enumerate(heightmap):
    for x, height in enumerate(row):
        if x > 0 and heightmap[y][x-1] <= height:
            continue
        if x < len(heightmap[y])-1 and heightmap[y][x+1] <= height:
            continue
        if y > 0 and heightmap[y-1][x] <= height:
            continue
        if y < len(heightmap)-1 and heightmap[y+1][x] <= height:
            continue
        totalRiskLevel += 1 + height

print(totalRiskLevel)
