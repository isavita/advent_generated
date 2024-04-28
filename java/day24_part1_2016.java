import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.*;

public class Main {

    public static void main(String[] args) throws IOException {
        String input = readFile("input.txt");
        System.out.println(cleaningRobot(input));
    }

    public static int cleaningRobot(String input) {
        String[] lines = input.split("\n");
        String[][] grid = new String[lines.length][];
        for (int i = 0; i < lines.length; i++) {
            grid[i] = lines[i].split("");
        }

        List<int[]> graph = new ArrayList<>();
        for (int r = 0; r < grid.length; r++) {
            for (int c = 0; c < grid[r].length; c++) {
                if (grid[r][c].matches("[0-9]")) {
                    String poi = grid[r][c];
                    int[] distancesFromPOI = bfsGetEdgeWeights(grid, new int[]{r, c});

                    if (graph.isEmpty()) {
                        for (int i = 0; i < distancesFromPOI.length; i++) {
                            graph.add(new int[distancesFromPOI.length]);
                        }
                    }
                    int index = Integer.parseInt(poi);
                    graph.set(index, distancesFromPOI);
                }
            }
        }

        return dfs(graph, 0, new boolean[graph.size()], false);
    }

    public static int[] bfsGetEdgeWeights(String[][] grid, int[] start) {
        Map<String, Integer> poiToDistance = new HashMap<>();
        poiToDistance.put(grid[start[0]][start[1]], 0);

        Queue<int[]> queue = new LinkedList<>();
        queue.add(new int[]{start[0], start[1], 0});
        boolean[][] visited = new boolean[grid.length][grid[0].length];
        while (!queue.isEmpty()) {
            int[] front = queue.poll();
            if (visited[front[0]][front[1]]) {
                continue;
            }
            visited[front[0]][front[1]] = true;

            if (grid[front[0]][front[1]].matches("[0-9]")) {
                poiToDistance.put(grid[front[0]][front[1]], front[2]);
            }
            for (int[] d : dirs) {
                int nextRow = front[0] + d[0];
                int nextCol = front[1] + d[1];

                if (nextRow >= 0 && nextRow < grid.length && nextCol >= 0 && nextCol < grid[0].length && !grid[nextRow][nextCol].equals("#")) {
                    queue.add(new int[]{nextRow, nextCol, front[2] + 1});
                }
            }
        }

        int[] distances = new int[poiToDistance.size()];
        for (Map.Entry<String, Integer> entry : poiToDistance.entrySet()) {
            distances[Integer.parseInt(entry.getKey())] = entry.getValue();
        }
        return distances;
    }

    public static final int[][] dirs = {
            {0, -1},
            {0, 1},
            {1, 0},
            {-1, 0}
    };

    public static int dfs(List<int[]> graph, int entryIndex, boolean[] visited, boolean returnToZero) {
        if (visited.length == visited.length - countFalse(visited)) {
            if (returnToZero) {
                return graph.get(entryIndex)[0];
            }
            return 0;
        }

        int minDistance = Integer.MAX_VALUE;
        for (int i = 0; i < graph.size(); i++) {
            if (!visited[i]) {
                visited[i] = true;

                int dist = graph.get(entryIndex)[i] + dfs(graph, i, visited, returnToZero);
                minDistance = Math.min(minDistance, dist);

                visited[i] = false;
            }
        }

        return minDistance;
    }

    public static int countFalse(boolean[] array) {
        int count = 0;
        for (boolean b : array) {
            if (!b) {
                count++;
            }
        }
        return count;
    }

    public static String readFile(String path) throws IOException {
        StringBuilder content = new StringBuilder();
        try (BufferedReader br = new BufferedReader(new FileReader(path))) {
            String line;
            while ((line = br.readLine()) != null) {
                content.append(line).append("\n");
            }
        }
        return content.toString().trim();
    }
}