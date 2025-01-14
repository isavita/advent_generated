
def solve() {
    def input = new File("input.txt").text.trim()
    def scanners = parseInput(input)

    def settled = [scanners[0]]
    settled[0].absoluteCoords = settled[0].relativeCoords
    settled[0].fillAbsoluteCoordsMap()

    def undetermined = scanners[1..-1]

    while (undetermined) {
        def updated = false
        for (int i = 0; i < undetermined.size(); i++) {
            def undet = undetermined[i]
            def maybeUpdated = findAbsoluteCoordsForScanner(undet, settled)
            if (maybeUpdated) {
                settled << maybeUpdated
                undetermined.remove(i)
                updated = true
                break
            }
        }
        if (!updated) {
            throw new RuntimeException("No scanner could be settled")
        }
    }

    def furthest = 0
    for (int i = 0; i < settled.size(); i++) {
        for (int j = i + 1; j < settled.size(); j++) {
            def s1 = settled[i]
            def s2 = settled[j]
            def manhattanDist = Math.abs(s1.x - s2.x) + Math.abs(s1.y - s2.y) + Math.abs(s1.z - s2.z)
            furthest = Math.max(furthest, manhattanDist)
        }
    }
    return furthest
}

class Scanner {
    int number
    int x = 0
    int y = 0
    int z = 0
    List<List<Integer>> relativeCoords
    List<List<List<Integer>>> rotations
    List<List<Integer>> absoluteCoords
    Set<List<Integer>> absoluteCoordsMap = [] as HashSet

    Scanner(int number, List<List<Integer>> relativeCoords) {
        this.number = number
        this.relativeCoords = relativeCoords
        fillRotations()
    }

    void fillAbsoluteCoordsMap() {
        absoluteCoordsMap.clear()
        if (!absoluteCoords) {
            throw new RuntimeException("absolute coords not set for scanner $number")
        }
        absoluteCoords.each { absoluteCoordsMap << it }
    }

    void fillRotations() {
        def posX = relativeCoords
        def dir2 = [], dir3 = [], dir4 = [], dir5 = [], dir6 = []
        posX.each { x, y, z ->
            dir2 << [x, -y, -z]
            dir3 << [x, -z, y]
            dir4 << [-y, -z, x]
            dir5 << [-x, -z, -y]
            dir6 << [y, -z, -x]
        }
        def sixRotations = [
            posX, dir2,
            dir3, dir4,
            dir5, dir6
        ]

        rotations = []
        sixRotations.each { rotation ->
            def r2 = [], r3 = [], r4 = []
            rotation.each { x, y, z ->
                r2 << [-y, x, z]
                r3 << [-x, -y, z]
                r4 << [y, -x, z]
            }
            rotations << rotation << r2 << r3 << r4
        }
    }
}

def findAbsoluteCoordsForScanner(Scanner undet, List<Scanner> settled) {
    for (def rotatedCoords : undet.rotations) {
        for (def set : settled) {
            for (def absCoord : set.absoluteCoords) {
                for (def relativeCoord : rotatedCoords) {
                    def unsettledAbsoluteCoords = makeAbsoluteCoordsList(absCoord, relativeCoord, rotatedCoords)
                    def matchingCount = unsettledAbsoluteCoords.count { set.absoluteCoordsMap.contains(it) }
                    if (matchingCount >= 12) {
                        undet.relativeCoords = rotatedCoords
                        undet.absoluteCoords = unsettledAbsoluteCoords
                        undet.fillAbsoluteCoordsMap()
                        undet.x = absCoord[0] - relativeCoord[0]
                        undet.y = absCoord[1] - relativeCoord[1]
                        undet.z = absCoord[2] - relativeCoord[2]
                        return undet
                    }
                }
            }
        }
    }
    return null
}

def makeAbsoluteCoordsList(List<Integer> absolute, List<Integer> relative, List<List<Integer>> relativeCoords) {
    def diff = [
        absolute[0] - relative[0],
        absolute[1] - relative[1],
        absolute[2] - relative[2]
    ]
    def absCoords = []
    relativeCoords.each { c ->
        absCoords << [
            diff[0] + c[0],
            diff[1] + c[1],
            diff[2] + c[2]
        ]
    }
    return absCoords
}

def parseInput(String input) {
    def scanners = []
    input.split("\n\n").each { rawScanner ->
        def lines = rawScanner.split("\n")
        def number = lines[0] =~ /--- scanner (\d+) ---/
        number = number[0][1].toInteger()

        def coords = []
        lines[1..-1].each { line ->
            def parts = line.split(",").collect { it.toInteger() }
            coords << parts
        }
        scanners << new Scanner(number, coords)
    }
    return scanners
}

println solve()
