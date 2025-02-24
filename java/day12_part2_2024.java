
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.*;

public class Solution {

    static int H, W;
    static char[][] graph;
    static int[][] moves = {{-1, 0}, {0, -1}, {1, 0}, {0, 1}};

    public static void main(String[] args) throws IOException {
        BufferedReader br = new BufferedReader(new FileReader("input.txt"));
        List<String> lines = new ArrayList<>();
        String line;
        while ((line = br.readLine()) != null) {
            if (!line.trim().isEmpty()) {
                lines.add(line.trim());
            }
        }
        br.close();

        H = lines.size();
        W = lines.get(0).length();
        graph = new char[H][W];
        for (int i = 0; i < H; i++) {
            graph[i] = lines.get(i).toCharArray();
        }

        long totalSum = 0;
        for (int y = 0; y < H; y++) {
            for (int x = 0; x < W; x++) {
                if (graph[y][x] == '.') {
                    continue;
                }

                int area = 0;
                char target = graph[y][x];
                Set<Integer> visited = new HashSet<>();
                Map<String, Set<Integer>> side = new HashMap<>();
                side.put("left", new HashSet<>());
                side.put("up", new HashSet<>());
                side.put("right", new HashSet<>());
                side.put("down", new HashSet<>());

                Queue<int[]> q = new LinkedList<>();
                q.offer(new int[]{x, y, -1});
                while (!q.isEmpty()) {
                    int[] curr = q.poll();
                    int cx = curr[0], cy = curr[1], label = curr[2];

                    if (graph[cy][cx] != target) {
                        if (label != -1 && !visited.contains(cx * W + cy)) {
                            addOuter(label, side, cx, cy);
                        }
                        continue;
                    }

                    visited.add(cx * W + cy);
                    area++;
                    graph[cy][cx] = '.';

                    for (int i = 0; i < 4; i++) {
                        int dx = moves[i][0], dy = moves[i][1];
                        int nx = cx + dx, ny = cy + dy;
                        if (nx >= 0 && nx < W && ny >= 0 && ny < H) {
                            q.offer(new int[]{nx, ny, i});
                        } else {
                            addOuter(i, side, nx, ny);
                        }
                    }
                }
                totalSum += (long) area * countOuter(side);
            }
        }
        System.out.println(totalSum);
    }

    static void addOuter(int labelIndex, Map<String, Set<Integer>> side, int x, int y) {
        String label = (labelIndex == 0) ? "left" : (labelIndex == 1) ? "up" : (labelIndex == 2) ? "right" : "down";
        int key = (labelIndex == 1 || labelIndex == 3) ? y * 10000 + x : x * 10000 + y;
        side.get(label).add(key);
    }

    static int countOuter(Map<String, Set<Integer>> side) {
        int outer = 0;
        for (String label : side.keySet()) {
            List<Integer> sortedKeys = new ArrayList<>(side.get(label));
            Collections.sort(sortedKeys);
            List<Integer> temp = new ArrayList<>();
            for (int key : sortedKeys) {
                if (!check(temp, key)) {
                    outer++;
                }
                temp.add(key);
            }
        }
        return outer;
    }

    static boolean check(List<Integer> ary, int key) {
        int i = key / 10000;
        int j = key % 10000;
        for (int[] move : new int[][]{{0, -1}, {0, 1}}) {
            int neighbor = (i + move[0]) * 10000 + (j + move[1]);
            if (ary.contains(neighbor)) {
                return true;
            }
        }
        return false;
    }
}
