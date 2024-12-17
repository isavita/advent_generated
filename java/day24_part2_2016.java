
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.*;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

public class Main {

    public static void main(String[] args) {
        try {
            String input = Files.readString(Paths.get("input.txt"));
            int result = cleaningRobot(input);
            System.out.println(result);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    static int cleaningRobot(String input) {
        List<List<String>> grid = Arrays.stream(input.split("\n"))
                .map(line -> Arrays.asList(line.split("")))
                .collect(Collectors.toList());

        int numPois = 0;
        for (List<String> row : grid) {
            for (String cell : row) {
                if (Pattern.matches("[0-9]", cell)) {
                    numPois++;
                }
            }
        }

        int[][] graph = new int[numPois][numPois];
        Map<Integer, int[]> poiPositions = new HashMap<>();
        int poiIndex = 0;

        for (int r = 0; r < grid.size(); r++) {
            for (int c = 0; c < grid.get(0).size(); c++) {
                String cell = grid.get(r).get(c);
                if (Pattern.matches("[0-9]", cell)) {
                    poiPositions.put(Integer.parseInt(cell), new int[]{r, c});
                }
            }
        }

        for (int i = 0; i < numPois; i++) {
            int[] start = poiPositions.get(i);
            int[] distancesFromPOI = bfsGetEdgeWeights(grid, start);
            graph[i] = distancesFromPOI;
        }

        return dfs(graph, 0, new HashSet<>(Collections.singletonList(0)), true);
    }

    static class BfsNode {
        int row, col, distance;

        BfsNode(int row, int col, int distance) {
            this.row = row;
            this.col = col;
            this.distance = distance;
        }
    }

    static int[] bfsGetEdgeWeights(List<List<String>> grid, int[] start) {
        Map<Integer, Integer> poiToDistance = new HashMap<>();
        poiToDistance.put(Integer.parseInt(grid.get(start[0]).get(start[1])), 0);

        Queue<BfsNode> queue = new LinkedList<>();
        queue.add(new BfsNode(start[0], start[1], 0));
        Set<String> visited = new HashSet<>();

        while (!queue.isEmpty()) {
            BfsNode front = queue.poll();
            String key = front.row + "," + front.col;
            if (visited.contains(key)) continue;
            visited.add(key);

            String cell = grid.get(front.row).get(front.col);
            if (Pattern.matches("[0-9]", cell)) {
                poiToDistance.put(Integer.parseInt(cell), front.distance);
            }

            int[][] dirs = {{0, -1}, {0, 1}, {1, 0}, {-1, 0}};
            for (int[] d : dirs) {
                int nextRow = front.row + d[0];
                int nextCol = front.col + d[1];

                if (nextRow >= 0 && nextRow < grid.size() && nextCol >= 0 && nextCol < grid.get(0).size() && !grid.get(nextRow).get(nextCol).equals("#")) {
                    queue.add(new BfsNode(nextRow, nextCol, front.distance + 1));
                }
            }
        }

        int[] distances = new int[poiToDistance.size()];
        for (Map.Entry<Integer, Integer> entry : poiToDistance.entrySet()) {
            distances[entry.getKey()] = entry.getValue();
        }
        return distances;
    }

    static int dfs(int[][] graph, int entryIndex, Set<Integer> visited, boolean returnToZero) {
        if (graph.length == visited.size()) {
            return returnToZero ? graph[entryIndex][0] : 0;
        }

        int minDistance = Integer.MAX_VALUE;
        for (int i = 0; i < graph[entryIndex].length; i++) {
            if (!visited.contains(i)) {
                visited.add(i);
                int dist = graph[entryIndex][i] + dfs(graph, i, visited, returnToZero);
                minDistance = Math.min(minDistance, dist);
                visited.remove(i);
            }
        }
        return minDistance;
    }
}
