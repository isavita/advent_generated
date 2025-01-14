
import java.io.File

class GardenRegion {
    int area
    int perimeter

    GardenRegion(int area, int perimeter) {
        this.area = area
        this.perimeter = perimeter
    }

    int getPrice() {
        return area * perimeter
    }
}

class GardenMap {
    static List<GardenRegion> findRegions(String[] map) {
        int rows = map.size()
        int cols = map[0].length()
        boolean[][] visited = new boolean[rows][cols]
        List<GardenRegion> regions = []

        for (int i = 0; i < rows; i++) {
            for (int j = 0; j < cols; j++) {
                if (!visited[i][j]) {
                    regions.add(calculateRegion(map, visited, i, j))
                }
            }
        }

        return regions
    }

    static GardenRegion calculateRegion(String[] map, boolean[][] visited, int startRow, int startCol) {
        char plantType = map[startRow][startCol]
        int area = 0
        int perimeter = 0
        Stack<int[]> stack = new Stack<int[]>()
        stack.push([startRow, startCol])

        while (!stack.empty()) {
            int[] current = stack.pop()
            int row = current[0]
            int col = current[1]

            if (visited[row][col]) continue

            visited[row][col] = true
            area++

            int[][] directions = [[-1, 0], [1, 0], [0, -1], [0, 1]]
            for (int[] dir : directions) {
                int newRow = row + dir[0]
                int newCol = col + dir[1]

                if (newRow >= 0 && newRow < map.size() && newCol >= 0 && newCol < map[0].length()) {
                    if (map[newRow][newCol] == plantType) {
                        if (!visited[newRow][newCol]) {
                            stack.push([newRow, newCol])
                        }
                    } else {
                        perimeter++
                    }
                } else {
                    perimeter++
                }
            }
        }

        return new GardenRegion(area, perimeter)
    }
}

def main() {
    File inputFile = new File('input.txt')
    String[] map = inputFile.readLines()

    List<GardenRegion> regions = GardenMap.findRegions(map)
    int totalPrice = regions.collect { it.getPrice() }.sum()

    println "Total price of fencing all regions: ${totalPrice}"
}

main()
