
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class SandSlabs {

    public static void main(String[] args) {
        try {
            List<Brick> bricks = readInput("input.txt");
            settleBricks(bricks);

            Map<Integer, Set<Integer>> supports = new HashMap<>();
            Map<Integer, Set<Integer>> supportedBy = new HashMap<>();
            buildSupportMaps(bricks, supports, supportedBy);

            int safeToDisintegrate = 0;
            for (int i = 0; i < bricks.size(); i++) {
                if (isSafeToDisintegrate(i, supports, supportedBy)) {
                    safeToDisintegrate++;
                }
            }
            System.out.println("Part 1: " + safeToDisintegrate);

            int totalChainReaction = 0;
            for (int i = 0; i < bricks.size(); i++) {
                totalChainReaction += calculateChainReaction(i, supports, supportedBy);
            }
            System.out.println("Part 2: " + totalChainReaction);

        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private static List<Brick> readInput(String filename) throws IOException {
        List<Brick> bricks = new ArrayList<>();
        BufferedReader reader = new BufferedReader(new FileReader(filename));
        String line;
        while ((line = reader.readLine()) != null) {
            bricks.add(parseBrick(line));
        }
        reader.close();
        return bricks;
    }

    private static Brick parseBrick(String line) {
        String[] parts = line.split("~");
        String[] startCoords = parts[0].split(",");
        String[] endCoords = parts[1].split(",");
        return new Brick(
                Integer.parseInt(startCoords[0]), Integer.parseInt(startCoords[1]), Integer.parseInt(startCoords[2]),
                Integer.parseInt(endCoords[0]), Integer.parseInt(endCoords[1]), Integer.parseInt(endCoords[2])
        );
    }

    private static void settleBricks(List<Brick> bricks) {
        bricks.sort((b1, b2) -> Integer.compare(b1.minZ, b2.minZ));
        for (int i = 0; i < bricks.size(); i++) {
            Brick brick = bricks.get(i);
            int maxZ = 1;
            for (int j = 0; j < i; j++) {
                Brick other = bricks.get(j);
                if (bricksOverlap(brick, other)) {
                    maxZ = Math.max(maxZ, other.maxZ + 1);
                }
            }
            brick.dropTo(maxZ);
        }
        bricks.sort((b1, b2) -> Integer.compare(b1.minZ, b2.minZ));
    }

    private static boolean bricksOverlap(Brick b1, Brick b2) {
        return Math.max(b1.minX, b2.minX) <= Math.min(b1.maxX, b2.maxX) &&
               Math.max(b1.minY, b2.minY) <= Math.min(b1.maxY, b2.maxY);
    }

    private static void buildSupportMaps(List<Brick> bricks, Map<Integer, Set<Integer>> supports, Map<Integer, Set<Integer>> supportedBy) {
        for (int i = 0; i < bricks.size(); i++) {
            supports.put(i, new HashSet<>());
            supportedBy.put(i, new HashSet<>());
        }

        for (int j = 0; j < bricks.size(); j++) {
            Brick upper = bricks.get(j);
            for (int i = 0; i < j; i++) {
                Brick lower = bricks.get(i);
                if (bricksOverlap(lower, upper) && upper.minZ == lower.maxZ + 1) {
                    supports.get(i).add(j);
                    supportedBy.get(j).add(i);
                }
            }
        }
    }

    private static boolean isSafeToDisintegrate(int brickIndex, Map<Integer, Set<Integer>> supports, Map<Integer, Set<Integer>> supportedBy) {
        for (int supportedBrick : supports.get(brickIndex)) {
            if (supportedBy.get(supportedBrick).size() == 1) {
                return false;
            }
        }
        return true;
    }

    private static int calculateChainReaction(int brickIndex, Map<Integer, Set<Integer>> supports, Map<Integer, Set<Integer>> supportedBy) {
        Set<Integer> fallen = new HashSet<>();
        fallen.add(brickIndex);
        Set<Integer> q = new HashSet<>(supports.get(brickIndex));
        
        while (!q.isEmpty()) {
            Set<Integer> nextQ = new HashSet<>();
            for (int b : q) {
                boolean allSupportingFallen = true;
                for (int supporter : supportedBy.get(b)) {
                    if (!fallen.contains(supporter)) {
                        allSupportingFallen = false;
                        break;
                    }
                }
                if (allSupportingFallen) {
                    fallen.add(b);
                    nextQ.addAll(supports.get(b));
                }
            }
            q = nextQ;
        }
        return fallen.size() - 1;
    }

    static class Brick {
        int minX, minY, minZ, maxX, maxY, maxZ;

        public Brick(int x1, int y1, int z1, int x2, int y2, int z2) {
            this.minX = Math.min(x1, x2);
            this.minY = Math.min(y1, y2);
            this.minZ = Math.min(z1, z2);
            this.maxX = Math.max(x1, x2);
            this.maxY = Math.max(y1, y2);
            this.maxZ = Math.max(z1, z2);
        }

        public void dropTo(int z) {
            int diff = minZ - z;
            minZ -= diff;
            maxZ -= diff;
        }
    }
}
