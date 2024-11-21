
import java.util.regex.Matcher
import java.util.regex.Pattern

class Cube {
    boolean isOn
    int x1, x2, y1, y2, z1, z2

    Cube(boolean isOn, int x1, int x2, int y1, int y2, int z1, int z2) {
        this.isOn = isOn
        this.x1 = x1
        this.x2 = x2
        this.y1 = y1
        this.y2 = y2
        this.z1 = z1
        this.z2 = z2
    }

    long volume() {
        long vol = (long)(x2 - x1 + 1) * (y2 - y1 + 1) * (z2 - z1 + 1)
        return isOn ? vol : -vol
    }

    Cube getIntersection(Cube c2) {
        int x1 = Math.max(x1, c2.x1)
        int x2 = Math.min(x2, c2.x2)
        int y1 = Math.max(y1, c2.y1)
        int y2 = Math.min(y2, c2.y2)
        int z1 = Math.max(z1, c2.z1)
        int z2 = Math.min(z2, c2.z2)

        if (x1 > x2 || y1 > y2 || z1 > z2) return null

        boolean intersectionState = (isOn && c2.isOn) ? false : (!isOn && !c2.isOn) ? true : c2.isOn
        return new Cube(intersectionState, x1, x2, y1, y2, z1, z2)
    }
}


def input = new File("input.txt").text.trim()
def pattern = Pattern.compile("(on|off) x=(-?\\d+)..(-?\\d+),y=(-?\\d+)..(-?\\d+),z=(-?\\d+)..(-?\\d+)")

def cubes = input.readLines().collect { line ->
    Matcher matcher = pattern.matcher(line)
    if (matcher.matches()) {
        new Cube(matcher.group(1) == "on", Integer.parseInt(matcher.group(2)), Integer.parseInt(matcher.group(3)),
                 Integer.parseInt(matcher.group(4)), Integer.parseInt(matcher.group(5)),
                 Integer.parseInt(matcher.group(6)), Integer.parseInt(matcher.group(7)))
    } else {
        throw new RuntimeException("Invalid input line: $line")
    }
}


def finalList = []
cubes.each { c ->
    def toAdd = []
    finalList.each { finalCube ->
        def intersection = finalCube.getIntersection(c)
        if (intersection) toAdd << intersection
    }
    if (c.isOn) toAdd << c
    finalList.addAll(toAdd)
}

long total = finalList.sum { it.volume() }
println total
