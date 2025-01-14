
def solve(String input) {
    def scanners = parseInput(input)
    def settled = [scanners[0]]
    settled[0].absoluteCoords = settled[0].relativeCoords
    settled[0].fillAbsoluteCoordsMap()
    def undetermined = scanners[1..-1]
    while (undetermined) {
        for (int i = 0; i < undetermined.size(); i++) {
            def undet = undetermined[i]
            def maybeUpdated = findAbsoluteCoordsForScanner(undet, settled)
            if (maybeUpdated.didUpdate) {
                settled << maybeUpdated.scanner
                undetermined.remove(i)
                break
            }
        }
    }
    def allBeacons = [:]
    for (def s : settled) {
        for (def c : s.absoluteCoordsMap.keySet()) {
            allBeacons[c] = true
        }
    }
    allBeacons.size()
}

class Scanner {
    int number
    int x, y, z
    List<List<Integer>> relativeCoords
    List<List<List<Integer>>> rotations
    List<List<Integer>> absoluteCoords
    Map<List<Integer>, Boolean> absoluteCoordsMap

    void fillAbsoluteCoordsMap() {
        absoluteCoordsMap = [:]
        if (!absoluteCoords) {
            throw new RuntimeException("absolute coords not set for scanner $number")
        }
        for (def ac : absoluteCoords) {
            absoluteCoordsMap[ac] = true
        }
    }

    void fillRotations() {
        def posX = relativeCoords
        def dir2 = [], dir3 = [], dir4 = [], dir5 = [], dir6 = []
        for (def c : posX) {
            def (x, y, z) = c
            dir2 << [x, -y, -z]
            dir3 << [x, -z, y]
            dir4 << [-y, -z, x]
            dir5 << [-x, -z, -y]
            dir6 << [y, -z, -x]
        }
        def sixRotations = [posX, dir2, dir3, dir4, dir5, dir6]
        def finalRotations = []
        for (def rotation : sixRotations) {
            def r2 = [], r3 = [], r4 = []
            for (def c : rotation) {
                def (x, y, z) = c
                r2 << [-y, x, z]
                r3 << [-x, -y, z]
                r4 << [y, -x, z]
            }
            finalRotations.addAll([rotation, r2, r3, r4])
        }
        rotations = finalRotations
    }
}

def findAbsoluteCoordsForScanner(Scanner undet, List<Scanner> settled) {
    for (def rotatedCoords : undet.rotations) {
        for (def set : settled) {
            for (def absCoord : set.absoluteCoords) {
                for (def relativeCoord : rotatedCoords) {
                    def unsettledAbsoluteCoords = makeAbsoluteCoordsList(absCoord, relativeCoord, rotatedCoords)
                    def matchingCount = 0
                    for (def ac : unsettledAbsoluteCoords) {
                        if (set.absoluteCoordsMap[ac]) {
                            matchingCount++
                        }
                    }
                    if (matchingCount >= 12) {
                        undet.relativeCoords = rotatedCoords
                        undet.absoluteCoords = unsettledAbsoluteCoords
                        undet.fillAbsoluteCoordsMap()
                        undet.x = absCoord[0] - relativeCoord[0]
                        undet.y = absCoord[1] - relativeCoord[1]
                        undet.z = absCoord[2] - relativeCoord[2]
                        return [scanner: undet, didUpdate: true]
                    }
                }
            }
        }
    }
    return [scanner: undet, didUpdate: false]
}

List<List<Integer>> makeAbsoluteCoordsList(List<Integer> absolute, List<Integer> relative, List<List<Integer>> relativeCoords) {
    def diff = [absolute[0] - relative[0], absolute[1] - relative[1], absolute[2] - relative[2]]
    def absCoords = []
    for (def c : relativeCoords) {
        absCoords << [diff[0] + c[0], diff[1] + c[1], diff[2] + c[2]]
    }
    absCoords
}

List<Scanner> parseInput(String input) {
    def ans = []
    for (def rawScanner : input.split("\n\n")) {
        def lines = rawScanner.split("\n")
        def number = lines[0].findAll(/(\d+)/)[0][0] as int
        def coords = []
        for (def line : lines[1..-1]) {
            def (x, y, z) = line.split(',').collect { it as int }
            coords << [x, y, z]
        }
        def sc = new Scanner(number: number, x: 0, y: 0, z: 0, relativeCoords: coords, absoluteCoords: null, absoluteCoordsMap: [:])
        sc.fillRotations()
        ans << sc
    }
    ans
}

def input = new File("input.txt").text.trim()
println solve(input)
