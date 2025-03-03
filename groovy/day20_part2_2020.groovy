
import java.nio.file.Files
import java.nio.file.Paths
import java.math.BigInteger

class Solution {

    static List<Map> parseTilesFromInput(String inputStr) {
        inputStr.split("\n\n").collect { block ->
            def lines = block.split("\n")
            def tileId = lines[0].split(" ")[1].substring(0, lines[0].split(" ")[1].length() - 1).toInteger()
            def contents = lines[1..-1].collect { it.toList() }
            [id: tileId, contents: contents]
        }
    }

    static String getCol(List<List<String>> grid, boolean firstCol) {
        grid.collect { firstCol ? it[0] : it[-1] }.join()
    }

    static String getRow(List<List<String>> grid, boolean firstRow) {
        (firstRow ? grid[0] : grid[-1]).join()
    }

    static List<List<String>> removeBordersFromGrid(List<List<String>> grid) {
        grid[1..-2].collect { it[1..-2] }
    }

    static List<List<List<String>>> allGridOrientations(List<List<String>> grid) {
        def orientations = [grid]
        4.times {
            orientations << rotateStringGrid(orientations[-1])
        }
        4.times { i ->
            orientations << mirrorStringGrid(orientations[i % 4])
        }
        orientations.unique()
    }

    static List<List<String>> rotateStringGrid(List<List<String>> grid) {
        (0..<grid[0].size()).collect { col ->
            (grid.size() - 1..0).collect { row ->
                grid[row][col]
            }
        }
    }

    static List<List<String>> mirrorStringGrid(List<List<String>> grid) {
        grid.collect { it.reverse() }
    }

    static List<Tuple2<Integer, Integer>> findMonsterCoords(List<List<String>> image) {
        def monster = [
            "                  # ".toList(),
            "#    ##    ##    ###".toList(),
            " #  #  #  #  #  #   ".toList(),
        ]
        def monsterOffsets = []
        for (int r = 0; r < monster.size(); r++) {
            for (int c = 0; c < monster[0].size(); c++) {
                if (monster[r][c] == "#") {
                    monsterOffsets << new Tuple2<>(r, c)
                }
            }
        }
        def monsterHeight = monster.size()
        def monsterLength = monster[0].size()

        def monsterStartingCoords = []
        for (int r = 0; r <= image.size() - monsterHeight; r++) {
            for (int c = 0; c <= image[0].size() - monsterLength; c++) {
                boolean monsterFound = true
                for (def offset : monsterOffsets) {
                    int dr = offset.first
                    int dc = offset.second
                    if (image[r + dr][c + dc] != "#") {
                        monsterFound = false
                        break
                    }
                }
                if (monsterFound) {
                    monsterStartingCoords << new Tuple2<>(r, c)
                }
            }
        }

        def monsterCoords = []
        for (def start : monsterStartingCoords) {
            int r = start.first
            int c = start.second
            for (def offset : monsterOffsets) {
                int dr = offset.first
                int dc = offset.second
                monsterCoords << new Tuple2<>(r + dr, c + dc)
            }
        }
        monsterCoords.unique()
    }

    static Map backtrackAssemble(List<Map> tiles, List<List<Map>> assembledTiles, Set<Integer> usedIndices, int edgeSize) {
        if (assembledTiles == null) {
            assembledTiles = (0..<edgeSize).collect { (0..<edgeSize).collect { null } }
        }

        for (int row = 0; row < edgeSize; row++) {
            for (int col = 0; col < edgeSize; col++) {
                if (assembledTiles[row][col] == null) {
                    for (int i = 0; i < tiles.size(); i++) {
                        if (!usedIndices.contains(i)) {
                            for (def opt : allGridOrientations(tiles[i].contents)) {
                                if (row != 0) {
                                    def currentTopRow = getRow(opt, true)
                                    def bottomOfAbove = getRow(assembledTiles[row - 1][col].contents, false)
                                    if (currentTopRow != bottomOfAbove) {
                                        continue
                                    }
                                }
                                if (col != 0) {
                                    def currentLeftCol = getCol(opt, true)
                                    def rightColOfLeft = getCol(assembledTiles[row][col - 1].contents, false)
                                    if (currentLeftCol != rightColOfLeft) {
                                        continue
                                    }
                                }

                                tiles[i].contents = opt
                                assembledTiles[row][col] = tiles[i]
                                usedIndices.add(i)
                                def result = backtrackAssemble(tiles, assembledTiles, usedIndices, edgeSize)
                                if (result != null) {
                                    return result
                                }
                                assembledTiles[row][col] = null
                                usedIndices.remove(i)
                            }
                        }
                    }

                    if (assembledTiles[row][col] == null) {
                        return null
                    }
                }
            }
        }

        [tiles:assembledTiles]
    }

    static int solve(String inputStr) {
        def tiles = parseTilesFromInput(inputStr)
        def edgeSize = Math.sqrt(tiles.size()) as int

        def assembledResult = backtrackAssemble(tiles, null, new HashSet<>(), edgeSize)

        if(assembledResult == null) {
            return -1;
        }

        def assembledTiles = assembledResult.tiles

        assembledTiles.each { row ->
            row.each { cell ->
                cell.contents = removeBordersFromGrid(cell.contents)
            }
        }

        def image = []
        for (int bigRow = 0; bigRow < edgeSize; bigRow++) {
            for (int subRow = 0; subRow < assembledTiles[0][0].contents.size(); subRow++) {
                image << []
                for (int bigCol = 0; bigCol < edgeSize; bigCol++) {
                    def subLine = assembledTiles[bigRow][bigCol].contents[subRow]
                    image[-1].addAll(subLine)
                }
            }
        }

        List<List<String>> finalImage = null;

        for (def opt : allGridOrientations(image)) {
            def monsterCoords = findMonsterCoords(opt)
            if (!monsterCoords.isEmpty()) {
                finalImage = opt;
                break;
            }
        }


        if(finalImage == null) {
            return -1;
        }

        def monsterCoords = findMonsterCoords(finalImage);
        for (def coord : monsterCoords) {
            finalImage[coord.first][coord.second] = "O"
        }
        
        def roughWatersCount = finalImage.sum { row -> row.count("#") }
        return roughWatersCount
    }

    static void main(String[] args) {
        def inputStr = Files.readString(Paths.get("input.txt")).trim()
        println solve(inputStr)
    }

    static class Tuple2<X, Y> {
        final X first;
        final Y second;

        Tuple2(X first, Y second) {
            this.first = first;
            this.second = second;
        }

        @Override
        boolean equals(Object o) {
            if (this.is(o)) return true
            if (getClass() != o.class) return false

            Tuple2<?, ?> tuple2 = (Tuple2<?, ?>) o

            if (first != null ? first != tuple2.first : tuple2.first != null) return false
            return second != null ? second == tuple2.second : tuple2.second == null
        }

        @Override
        int hashCode() {
            int result = first != null ? first.hashCode() : 0
            result = 31 * result + (second != null ? second.hashCode() : 0)
            return result
        }
    }
}
