
import java.nio.file.Files
import java.nio.file.Paths

class ConwayCubes {

    static int solve(List<String> initialGrid, int dimensions, int cycles) {
        Set<List<Integer>> activeCubes = new HashSet<>()

        // Initialize active cubes from initial grid
        for (int y = 0; y < initialGrid.size(); y++) {
            for (int x = 0; x < initialGrid[y].length(); x++) {
                if (initialGrid[y][x] == '#') {
                    List<Integer> coords = new ArrayList<>()
                    coords.add(x)
                    coords.add(y)
                    for (int i = 2; i < dimensions; i++) {
                        coords.add(0)
                    }
                    activeCubes.add(coords)
                }
            }
        }

        for (int cycle = 0; cycle < cycles; cycle++) {
            Set<List<Integer>> newActiveCubes = new HashSet<>()
            Set<List<Integer>> cubesToCheck = new HashSet<>(activeCubes)

            // Add neighbors of active cubes to cubesToCheck
            activeCubes.each { cube ->
                getNeighbors(cube, dimensions).each { neighbor ->
                    cubesToCheck.add(neighbor)
                }
            }

            cubesToCheck.each { cube ->
                int activeNeighbors = countActiveNeighbors(cube, activeCubes, dimensions)
                if (activeCubes.contains(cube)) {
                    if (activeNeighbors == 2 || activeNeighbors == 3) {
                        newActiveCubes.add(cube)
                    }
                } else {
                    if (activeNeighbors == 3) {
                        newActiveCubes.add(cube)
                    }
                }
            }
            activeCubes = newActiveCubes
        }

        return activeCubes.size()
    }

    static List<List<Integer>> getNeighbors(List<Integer> cube, int dimensions) {
        List<List<Integer>> neighbors = new ArrayList<>()
        List<Integer> deltas = new ArrayList<>()
        for (int i = 0; i < dimensions; i++) {
            deltas.add(0)
        }

        generateDeltas(neighbors, deltas, 0, dimensions, cube)
        neighbors.removeIf { it.equals(cube) } // Remove the cube itself from neighbors

        return neighbors
    }

    static void generateDeltas(List<List<Integer>> neighbors, List<Integer> deltas, int index, int dimensions, List<Integer> cube) {
        if (index == dimensions) {
            List<Integer> neighbor = new ArrayList<>()
            for (int i = 0; i < dimensions; i++) {
                neighbor.add(cube.get(i) + deltas.get(i))
            }
            neighbors.add(neighbor)
            return
        }

        for (int delta = -1; delta <= 1; delta++) {
            deltas.set(index, delta)
            generateDeltas(neighbors, deltas, index + 1, dimensions, cube)
        }
    }


    static int countActiveNeighbors(List<Integer> cube, Set<List<Integer>> activeCubes, int dimensions) {
        int count = 0
        getNeighbors(cube, dimensions).each { neighbor ->
            if (activeCubes.contains(neighbor)) {
                count++
            }
        }
        return count
    }


    static void main(String[] args) {
        List<String> initialGrid = Files.readAllLines(Paths.get("input.txt"))

        int part1 = solve(initialGrid, 3, 6)
        println "Part 1: " + part1

        int part2 = solve(initialGrid, 4, 6)
        println "Part 2: " + part2
    }
}
