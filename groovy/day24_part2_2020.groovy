
class Solution {
    static void main(String[] args) {
        def directions = ['e': [1, 0], 'se': [0, -1], 'sw': [-1, -1], 'w': [-1, 0], 'nw': [0, 1], 'ne': [1, 1]]
        def tiles = [:].withDefault { false }
        new File("input.txt").readLines().each { line ->
            def x = 0, y = 0
            def i = 0
            while (i < line.length()) {
                def dir = directions[line[i]] ? line[i] : line[i..i+1]
                def (dx, dy) = directions[dir]
                x += dx
                y += dy
                i += dir.length()
            }
            tiles[[x, y]] = !tiles[[x, y]]
        }

        100.times {
            def newTiles = [:].withDefault { false }
            (-150..150).each { x ->
                (-150..150).each { y ->
                    def adjCount = directions.values().sum { dx, dy -> tiles[[x + dx, y + dy]] ? 1 : 0 }
                    if (tiles[[x, y]]) {
                        newTiles[[x, y]] = adjCount in [1, 2]
                    } else {
                        newTiles[[x, y]] = adjCount == 2
                    }
                }
            }
            tiles = newTiles
        }
      
      println tiles.values().count { it }
    }
}
