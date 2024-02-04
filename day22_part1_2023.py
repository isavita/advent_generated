
import sys

class Coord:
    def __init__(self, x, y, z):
        self.x = x
        self.y = y
        self.z = z

class Brick:
    def __init__(self, mini, maxi):
        self.mini = mini
        self.maxi = maxi
        self.basedOn = []
        self.support = []

def parseInput(input):
    bricks = []
    for line in input:
        values = line.split("~")
        mini_values = list(map(int, values[0].split(",")))
        maxi_values = list(map(int, values[1].split(",")))
        mini = Coord(mini_values[0], mini_values[1], mini_values[2])
        maxi = Coord(maxi_values[0], maxi_values[1], maxi_values[2])
        brick = Brick(mini, maxi)
        bricks.append(brick)
    return bricks

def settle(bricks):
    bricks.sort(key=lambda x: x.maxi.z)
    
    for i in range(len(bricks)):
        supportZ = 0
        basedBricks = []
        
        for j in range(i-1, -1, -1):
            isIntersectingX = max(bricks[i].mini.x, bricks[j].mini.x) <= min(bricks[i].maxi.x, bricks[j].maxi.x)
            isIntersectingY = max(bricks[i].mini.y, bricks[j].mini.y) <= min(bricks[i].maxi.y, bricks[j].maxi.y)
            isIntersecting = isIntersectingX and isIntersectingY
            if isIntersecting:
                if bricks[j].maxi.z == supportZ:
                    basedBricks.append(bricks[j])
                elif bricks[j].maxi.z > supportZ:
                    supportZ = bricks[j].maxi.z
                    basedBricks = [bricks[j]]
        
        bricks[i].basedOn = basedBricks
        for basedBrick in basedBricks:
            basedBrick.support.append(bricks[i])
        
        deltaZ = bricks[i].maxi.z - bricks[i].mini.z
        bricks[i].mini.z = supportZ + 1
        bricks[i].maxi.z = bricks[i].mini.z + deltaZ

def solve(input):
    bricks = parseInput(input)
    settle(bricks)
    
    cnt = 0
    for brick in bricks:
        isDisintegratable = True
        for supportedBrick in brick.support:
            if len(supportedBrick.basedOn) < 2:
                isDisintegratable = False
                break
        if isDisintegratable:
            cnt += 1
    return cnt

with open("input.txt", "r") as file:
    input_data = file.read().splitlines()

print(solve(input_data))
