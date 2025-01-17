
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.Arrays;
import java.util.PriorityQueue;

public class ModeMaze {

    private static final int ROCKY = 0;
    private static final int WET = 1;
    private static final int NARROW = 2;

    private static final int TORCH = 0;
    private static final int CLIMBING_GEAR = 1;
    private static final int NEITHER = 2;

    private static final int[][] VALID_TOOLS = {
            {TORCH, CLIMBING_GEAR}, // ROCKY
            {CLIMBING_GEAR, NEITHER}, // WET
            {TORCH, NEITHER}          // NARROW
    };

    public static void main(String[] args) {
        try (BufferedReader reader = new BufferedReader(new FileReader("input.txt"))) {
            String depthLine = reader.readLine();
            String targetLine = reader.readLine();

            int depth = Integer.parseInt(depthLine.split(": ")[1]);
            String[] targetCoords = targetLine.split(": ")[1].split(",");
            int targetX = Integer.parseInt(targetCoords[0]);
            int targetY = Integer.parseInt(targetCoords[1]);

            int riskLevel = calculateRiskLevel(depth, targetX, targetY);
            System.out.println("Total risk level: " + riskLevel);

            int minMinutes = findFastestPath(depth, targetX, targetY);
            System.out.println("Fewest minutes to reach target: " + minMinutes);

        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private static int calculateRiskLevel(int depth, int targetX, int targetY) {
        int[][] erosionLevels = new int[targetY + 1][targetX + 1];
        int riskLevel = 0;

        for (int y = 0; y <= targetY; y++) {
            for (int x = 0; x <= targetX; x++) {
                erosionLevels[y][x] = calculateErosionLevel(x, y, depth, targetX, targetY, erosionLevels);
                riskLevel += erosionLevels[y][x] % 3;
            }
        }

        return riskLevel;
    }

    private static int calculateErosionLevel(int x, int y, int depth, int targetX, int targetY, int[][] erosionLevels) {
        int geologicIndex;
        if ((x == 0 && y == 0) || (x == targetX && y == targetY)) {
            geologicIndex = 0;
        } else if (y == 0) {
            geologicIndex = x * 16807;
        } else if (x == 0) {
            geologicIndex = y * 48271;
        } else {
            geologicIndex = erosionLevels[y][x - 1] * erosionLevels[y - 1][x];
        }
        return (geologicIndex + depth) % 20183;
    }

    private static int findFastestPath(int depth, int targetX, int targetY) {
        int maxX = targetX + 50;
        int maxY = targetY + 50;
        int[][] erosionLevels = new int[maxY + 1][maxX + 1];
        int[][] regionTypes = new int[maxY + 1][maxX + 1];

        for (int y = 0; y <= maxY; y++) {
            for (int x = 0; x <= maxX; x++) {
                erosionLevels[y][x] = calculateErosionLevel(x, y, depth, targetX, targetY, erosionLevels);
                regionTypes[y][x] = erosionLevels[y][x] % 3;
            }
        }

        int[][][] dist = new int[maxY + 1][maxX + 1][3];
        for (int[][] row : dist) {
            for (int[] col : row) {
                Arrays.fill(col, Integer.MAX_VALUE);
            }
        }
        dist[0][0][TORCH] = 0;

        PriorityQueue<State> pq = new PriorityQueue<>();
        pq.offer(new State(0, 0, TORCH, 0));

        int[][] dirs = {{-1, 0}, {1, 0}, {0, -1}, {0, 1}};

        while (!pq.isEmpty()) {
            State curr = pq.poll();

            if (curr.x == targetX && curr.y == targetY && curr.tool == TORCH) {
                return curr.time;
            }

            if (curr.time > dist[curr.y][curr.x][curr.tool]) {
                continue;
            }

            // Change tool
            for (int newTool : VALID_TOOLS[regionTypes[curr.y][curr.x]]) {
                if (newTool != curr.tool) {
                    int newTime = curr.time + 7;
                    if (newTime < dist[curr.y][curr.x][newTool]) {
                        dist[curr.y][curr.x][newTool] = newTime;
                        pq.offer(new State(curr.x, curr.y, newTool, newTime));
                    }
                }
            }

            // Move
            for (int[] dir : dirs) {
                int newX = curr.x + dir[0];
                int newY = curr.y + dir[1];

                if (newX >= 0 && newX <= maxX && newY >= 0 && newY <= maxY) {
                    if (isValidTool(regionTypes[newY][newX], curr.tool)) {
                        int newTime = curr.time + 1;
                        if (newTime < dist[newY][newX][curr.tool]) {
                            dist[newY][newX][curr.tool] = newTime;
                            pq.offer(new State(newX, newY, curr.tool, newTime));
                        }
                    }
                }
            }
        }

        return -1; // Should not reach here
    }

    private static boolean isValidTool(int regionType, int tool) {
        for (int validTool : VALID_TOOLS[regionType]) {
            if (tool == validTool) {
                return true;
            }
        }
        return false;
    }

    static class State implements Comparable<State> {
        int x, y, tool, time;

        public State(int x, int y, int tool, int time) {
            this.x = x;
            this.y = y;
            this.tool = tool;
            this.time = time;
        }

        @Override
        public int compareTo(State other) {
            return Integer.compare(this.time, other.time);
        }
    }
}
