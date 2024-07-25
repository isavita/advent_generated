
class Coord {
    int x, y, z
}

class Brick {
    Coord mini, maxi
    List<Brick> basedOn = []
    List<Brick> support = []
}

def parseInput(input) {
    input.collect { line ->
        def coords = line.split("~")
        def mini = coords[0].split(",").collect { it.toInteger() }
        def maxi = coords[1].split(",").collect { it.toInteger() }
        new Brick(mini: new Coord(x: mini[0], y: mini[1], z: mini[2]), 
                  maxi: new Coord(x: maxi[0], y: maxi[1], z: maxi[2]))
    }
}

def settle(bricks) {
    bricks.sort { it.maxi.z }
    bricks.eachWithIndex { brick, i ->
        int supportZ = 0
        List<Brick> basedBricks = []

        for (int j = i - 1; j >= 0; j--) {
            def isIntersectingX = Math.max(brick.mini.x, bricks[j].mini.x) <= Math.min(brick.maxi.x, bricks[j].maxi.x)
            def isIntersectingY = Math.max(brick.mini.y, bricks[j].mini.y) <= Math.min(brick.maxi.y, bricks[j].maxi.y)
            if (isIntersectingX && isIntersectingY) {
                if (bricks[j].maxi.z == supportZ) {
                    basedBricks << bricks[j]
                } else if (bricks[j].maxi.z > supportZ) {
                    supportZ = bricks[j].maxi.z
                    basedBricks = [bricks[j]]
                }
            }
        }

        brick.basedOn = basedBricks
        basedBricks.each { it.support << brick }

        int deltaZ = brick.maxi.z - brick.mini.z
        brick.mini.z = supportZ + 1
        brick.maxi.z = brick.mini.z + deltaZ
    }
}

def solve(input) {
    def bricks = parseInput(input)
    settle(bricks)

    int cnt = 0
    bricks.each { brick ->
        def fallingBricks = [:]
        brick.support.each { supportedBrick ->
            if (supportedBrick.basedOn.size() == 1) {
                def allSupportedBricks = [supportedBrick]
                while (allSupportedBricks) {
                    def supportedBrick0 = allSupportedBricks.remove(0)
                    boolean isFalling = true
                    supportedBrick0.basedOn.each { basedBrick ->
                        if (basedBrick != brick && !fallingBricks.containsKey(basedBrick)) {
                            isFalling = false
                        }
                    }
                    if (isFalling) {
                        fallingBricks[supportedBrick0] = true
                        allSupportedBricks.addAll(supportedBrick0.support)
                    }
                }
            }
        }
        cnt += fallingBricks.size()
    }
    cnt
}

def readFile(fileName) {
    new File(fileName).readLines().collect { it.trim() }
}

def input = readFile("input.txt")
println solve(input)
