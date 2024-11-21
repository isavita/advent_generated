
class MazeExplorer {
    static final int FAVORITE_NUMBER = 1362

    static boolean isWall(int x, int y) {
        int num = x*x + 3*x + 2*x*y + y + y*y + FAVORITE_NUMBER
        return Integer.bitCount(num) % 2 != 0
    }

    static int exploreReachableLocations(int maxSteps) {
        def visited = new HashSet()
        def queue = [[x: 1, y: 1]]
        visited.add([x: 1, y: 1])

        for (int step = 0; step < maxSteps; step++) {
            def nextQueue = []
            for (def point in queue) {
                [[1, 0], [-1, 0], [0, 1], [0, -1]].each { dx, dy ->
                    def next = [x: point.x + dx, y: point.y + dy]
                    if (next.x >= 0 && next.y >= 0 && !isWall(next.x, next.y) && !visited.contains(next)) {
                        visited.add(next)
                        nextQueue.add(next)
                    }
                }
            }
            queue = nextQueue
        }

        return visited.size()
    }

    static void main(String[] args) {
        println exploreReachableLocations(50)
    }
}
