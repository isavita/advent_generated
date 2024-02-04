
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
        x1, y1, z1, x2, y2, z2 = map(int, line.split('~')[0].split(',') + line.split('~')[1].split(','))
        brick = Brick(Coord(x1, y1, z1), Coord(x2, y2, z2))
        bricks.append(brick)
    return bricks

def settle(bricks):
    bricks.sort(key=lambda x: x.maxi.z)
    
    for i, brick in enumerate(bricks):
        supportZ = 0
        basedBricks = []
        
        for j in range(i-1, -1, -1):
            isIntersectingX = max(brick.mini.x, bricks[j].mini.x) <= min(brick.maxi.x, bricks[j].maxi.x)
            isIntersectingY = max(brick.mini.y, bricks[j].mini.y) <= min(brick.maxi.y, bricks[j].maxi.y)
            isIntersecting = isIntersectingX and isIntersectingY
            if isIntersecting:
                if bricks[j].maxi.z == supportZ:
                    basedBricks.append(bricks[j])
                elif bricks[j].maxi.z > supportZ:
                    supportZ = bricks[j].maxi.z
                    basedBricks = [bricks[j]]
        
        brick.basedOn = basedBricks
        for basedBrick in basedBricks:
            basedBrick.support.append(brick)
        
        deltaZ = brick.maxi.z - brick.mini.z
        brick.mini.z = supportZ + 1
        brick.maxi.z = brick.mini.z + deltaZ

def solve(input):
    bricks = parseInput(input)
    settle(bricks)
    
    cnt = 0
    for brick in bricks:
        fallingBricks = {}
        for supportedBrick in brick.support:
            if len(supportedBrick.basedOn) == 1:
                allSupportedBricks = [supportedBrick]
                while allSupportedBricks:
                    supportedBrick0 = allSupportedBricks.pop(0)
                    
                    isFalling = True
                    for basedBrick in supportedBrick0.basedOn:
                        if basedBrick != brick and basedBrick not in fallingBricks:
                            isFalling = False
                            break
                    
                    if isFalling:
                        fallingBricks[supportedBrick0] = None
                        allSupportedBricks.extend(supportedBrick0.support)
        
        cnt += len(fallingBricks)
    
    return cnt

def readFile(fileName):
    with open(fileName, 'r') as file:
        return file.read().strip().split('\n')

if __name__ == "__main__":
    input = readFile("input.txt")
    print(solve(input))
