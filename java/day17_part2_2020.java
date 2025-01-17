
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashSet;
import java.util.Set;

public class ConwayCubes {

    public static void main(String[] args) throws IOException {
        Set<Point> activeCubes3D = readInput("input.txt", false);
        Set<Point> activeCubes4D = readInput("input.txt", true);

        for (int i = 0; i < 6; i++) {
            activeCubes3D = simulateCycle(activeCubes3D, false);
            activeCubes4D = simulateCycle(activeCubes4D, true);
        }

        System.out.println("Part 1: " + activeCubes3D.size());
        System.out.println("Part 2: " + activeCubes4D.size());
    }

    private static Set<Point> readInput(String filename, boolean is4D) throws IOException {
        Set<Point> activeCubes = new HashSet<>();
        try (BufferedReader br = new BufferedReader(new FileReader(filename))) {
            String line;
            int y = 0;
            while ((line = br.readLine()) != null) {
                for (int x = 0; x < line.length(); x++) {
                    if (line.charAt(x) == '#') {
                        activeCubes.add(new Point(x, y, 0, is4D ? 0 : null));
                    }
                }
                y++;
            }
        }
        return activeCubes;
    }

    private static Set<Point> simulateCycle(Set<Point> activeCubes, boolean is4D) {
        Set<Point> nextActiveCubes = new HashSet<>();
        Set<Point> neighborsToCheck = new HashSet<>();

        for (Point cube : activeCubes) {
            int activeNeighbors = countActiveNeighbors(cube, activeCubes, is4D);
            if (activeNeighbors == 2 || activeNeighbors == 3) {
                nextActiveCubes.add(cube);
            }
            neighborsToCheck.addAll(getNeighbors(cube, is4D));
        }

        for (Point neighbor : neighborsToCheck) {
            if (!activeCubes.contains(neighbor)) {
                if (countActiveNeighbors(neighbor, activeCubes, is4D) == 3) {
                    nextActiveCubes.add(neighbor);
                }
            }
        }

        return nextActiveCubes;
    }

    private static int countActiveNeighbors(Point cube, Set<Point> activeCubes, boolean is4D) {
        int count = 0;
        for (Point neighbor : getNeighbors(cube, is4D)) {
            if (activeCubes.contains(neighbor)) {
                count++;
            }
        }
        return count;
    }

    private static Set<Point> getNeighbors(Point cube, boolean is4D) {
        Set<Point> neighbors = new HashSet<>();
        for (int dx = -1; dx <= 1; dx++) {
            for (int dy = -1; dy <= 1; dy++) {
                for (int dz = -1; dz <= 1; dz++) {
                    if (is4D) {
                        for (int dw = -1; dw <= 1; dw++) {
                            if (dx != 0 || dy != 0 || dz != 0 || dw != 0) {
                                neighbors.add(new Point(cube.x + dx, cube.y + dy, cube.z + dz, cube.w + dw));
                            }
                        }
                    } else {
                        if (dx != 0 || dy != 0 || dz != 0) {
                            neighbors.add(new Point(cube.x + dx, cube.y + dy, cube.z + dz, null));
                        }
                    }
                }
            }
        }
        return neighbors;
    }

    static class Point {
        int x, y, z;
        Integer w;

        public Point(int x, int y, int z, Integer w) {
            this.x = x;
            this.y = y;
            this.z = z;
            this.w = w;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            Point point = (Point) o;
            return x == point.x && y == point.y && z == point.z && (w == null ? point.w == null : w.equals(point.w));
        }

        @Override
        public int hashCode() {
            int result = x;
            result = 31 * result + y;
            result = 31 * result + z;
            result = 31 * result + (w != null ? w.hashCode() : 0);
            return result;
        }
    }
}
