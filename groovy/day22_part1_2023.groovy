
class Coord {
    int x
    int y
    int z
}

class Brick {
    Coord mini
    Coord maxi
    List<Brick> basedOn = []
    List<Brick> support = []
}

List<Brick> parseInput(List<String> input) {
    input.collect { line ->
        def parts = line.split(/[~,]/).collect { it.toInteger() }
        new Brick(mini: new Coord(x: parts[0], y: parts[1], z: parts[2]),
                maxi: new Coord(x: parts[3], y: parts[4], z: parts[5]))
    }
}

void settle(List<Brick> bricks) {
    bricks.sort { it.maxi.z }

    bricks.eachWithIndex { brick, i ->
        int supportZ = 0
        List<Brick> basedBricks = []

        for (int j = i - 1; j >= 0; j--) {
            def other = bricks[j]
            boolean isIntersectingX = Math.max(brick.mini.x, other.mini.x) <= Math.min(brick.maxi.x, other.maxi.x)
            boolean isIntersectingY = Math.max(brick.mini.y, other.mini.y) <= Math.min(brick.maxi.y, other.maxi.y)
            if (isIntersectingX && isIntersectingY) {
                if (other.maxi.z == supportZ) {
                    basedBricks << other
                } else if (other.maxi.z > supportZ) {
                    supportZ = other.maxi.z
                    basedBricks = [other]
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

int solve(List<String> input) {
    List<Brick> bricks = parseInput(input)
    settle(bricks)

    bricks.count { brick ->
        brick.support.every { it.basedOn.size() > 1 }
    }
}

def input = new File("input.txt").readLines()
println solve(input)
