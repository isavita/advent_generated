
import java.io.*;
import java.util.*;
import java.util.regex.*;

public class Nanobots {

    static class Nanobot {
        int x, y, z, r;

        Nanobot(int x, int y, int z, int r) {
            this.x = x;
            this.y = y;
            this.z = z;
            this.r = r;
        }
    }

    static class Cube implements Comparable<Cube> {
        int count, distance, size, x, y, z;

        Cube(int count, int distance, int size, int x, int y, int z) {
            this.count = count;
            this.distance = distance;
            this.size = size;
            this.x = x;
            this.y = y;
            this.z = z;
        }

        @Override
        public int compareTo(Cube other) {
            if (this.count != other.count) {
                return other.count - this.count;
            }
            return this.distance - other.distance;
        }
    }

    static List<Nanobot> parseInput(String filePath) throws IOException {
        List<Nanobot> nanobots = new ArrayList<>();
        Pattern pattern = Pattern.compile("pos=<(-?\\d+),(-?\\d+),(-?\\d+)>, r=(\\d+)");
        try (BufferedReader br = new BufferedReader(new FileReader(filePath))) {
            String line;
            while ((line = br.readLine()) != null) {
                Matcher matcher = pattern.matcher(line);
                if (matcher.find()) {
                    int x = Integer.parseInt(matcher.group(1));
                    int y = Integer.parseInt(matcher.group(2));
                    int z = Integer.parseInt(matcher.group(3));
                    int r = Integer.parseInt(matcher.group(4));
                    nanobots.add(new Nanobot(x, y, z, r));
                }
            }
        }
        return nanobots;
    }

    static int manhattanDistance(int x1, int y1, int z1, int x2, int y2, int z2) {
        return Math.abs(x1 - x2) + Math.abs(y1 - y2) + Math.abs(z1 - z2);
    }

    static int partOne(List<Nanobot> nanobots) {
        Nanobot strongest = Collections.max(nanobots, Comparator.comparingInt(bot -> bot.r));
        int count = 0;
        for (Nanobot bot : nanobots) {
            if (manhattanDistance(strongest.x, strongest.y, strongest.z, bot.x, bot.y, bot.z) <= strongest.r) {
                count++;
            }
        }
        return count;
    }

    static int minDistanceToOrigin(int x, int y, int z, int size) {
        int dx = (x > 0) ? x : ((x + size - 1 < 0) ? -(x + size - 1) : 0);
        int dy = (y > 0) ? y : ((y + size - 1 < 0) ? -(y + size - 1) : 0);
        int dz = (z > 0) ? z : ((z + size - 1 < 0) ? -(z + size - 1) : 0);
        return dx + dy + dz;
    }

    static int partTwo(List<Nanobot> nanobots) {
        int minX = nanobots.stream().mapToInt(bot -> bot.x).min().getAsInt();
        int maxX = nanobots.stream().mapToInt(bot -> bot.x).max().getAsInt();
        int minY = nanobots.stream().mapToInt(bot -> bot.y).min().getAsInt();
        int maxY = nanobots.stream().mapToInt(bot -> bot.y).max().getAsInt();
        int minZ = nanobots.stream().mapToInt(bot -> bot.z).min().getAsInt();
        int maxZ = nanobots.stream().mapToInt(bot -> bot.z).max().getAsInt();

        int size = 1;
        while (size < Math.max(maxX - minX, Math.max(maxY - minY, maxZ - minZ))) {
            size *= 2;
        }

        PriorityQueue<Cube> heap = new PriorityQueue<>();
        heap.add(new Cube(0, minDistanceToOrigin(minX, minY, minZ, size), size, minX, minY, minZ));

        int bestDistance = 0;
        int bestCount = -1;

        while (!heap.isEmpty()) {
            Cube cube = heap.poll();

            if (cube.size == 1) {
                if (cube.count > bestCount || (cube.count == bestCount && cube.distance < bestDistance)) {
                    bestCount = cube.count;
                    bestDistance = cube.distance;
                    break;
                }
                continue;
            }

            int half = cube.size / 2;
            for (int dx = 0; dx <= half; dx += half) {
                for (int dy = 0; dy <= half; dy += half) {
                    for (int dz = 0; dz <= half; dz += half) {
                        int nx = cube.x + dx;
                        int ny = cube.y + dy;
                        int nz = cube.z + dz;
                        int newSize = Math.max(1, half);

                        int count = 0;
                        for (Nanobot bot : nanobots) {
                            int d = 0;
                            if (bot.x < nx)             d += nx - bot.x;
                            else if (bot.x > nx + newSize - 1) d += bot.x - (nx + newSize - 1);
                            if (bot.y < ny)             d += ny - bot.y;
                            else if (bot.y > ny + newSize - 1) d += bot.y - (ny + newSize - 1);
                            if (bot.z < nz)             d += nz - bot.z;
                            else if (bot.z > nz + newSize - 1) d += bot.z - (nz + newSize - 1);
                            
                            if (d <= bot.r)  count++;
                        }

                        int distance = minDistanceToOrigin(nx, ny, nz, newSize);
                        heap.add(new Cube(count, distance, newSize, nx, ny, nz));
                    }
                }
            }
        }

        return bestDistance;
    }

    public static void main(String[] args) throws IOException {
        List<Nanobot> nanobots = parseInput("input.txt");
        System.out.println(partOne(nanobots));
        System.out.println(partTwo(nanobots));
    }
}
