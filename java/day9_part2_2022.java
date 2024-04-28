import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

public class Main {
    public static void main(String[] args) throws IOException {
        String input = readFile("input.txt");
        System.out.println(visited(input, 10));
    }

    public static int visited(String input, int ropeLen) {
        int[][] rope = new int[ropeLen][2];
        Set<String> visited = new HashSet<>();
        for (String line : input.split("\n")) {
            String[] parts = line.split(" ");
            char dir = parts[0].charAt(0);
            int n = Integer.parseInt(parts[1]);
            int dx = 0, dy = 0;
            switch (dir) {
                case 'N':
                case 'U':
                case '^':
                    dy = 1;
                    break;
                case 'S':
                case 'D':
                case 'v':
                    dy = -1;
                    break;
                case 'E':
                case 'R':
                case '>':
                    dx = 1;
                    break;
                case 'W':
                case 'L':
                case '<':
                    dx = -1;
                    break;
            }
            for (int i = 0; i < n; i++) {
                rope[0][0] += dx;
                rope[0][1] += dy;
                for (int j = 1; j < ropeLen; j++) {
                    rope[j] = next(rope[j - 1], rope[j]);
                }
                visited.add(rope[ropeLen - 1][0] + "," + rope[ropeLen - 1][1]);
            }
        }
        return visited.size();
    }

    public static int[] next(int[] head, int[] tail) {
        if (Math.abs(head[0] - tail[0]) <= 1 && Math.abs(head[1] - tail[1]) <= 1) {
            return tail;
        }
        int dx = sign(head[0] - tail[0]);
        int dy = sign(head[1] - tail[1]);
        return new int[]{tail[0] + dx, tail[1] + dy};
    }

    public static int sign(int n) {
        if (n == 0) {
            return 0;
        }
        return n < 0 ? -1 : 1;
    }

    public static String readFile(String filename) throws IOException {
        StringBuilder sb = new StringBuilder();
        try (BufferedReader br = new BufferedReader(new FileReader(filename))) {
            String line;
            while ((line = br.readLine()) != null) {
                sb.append(line).append("\n");
            }
        }
        return sb.toString();
    }
}