
import java.io.File;
import java.io.FileNotFoundException;
import java.util.*;

public class AntinodeCounter {

    public static int gcd(int a, int b) {
        if (b == 0) {
            return a < 0 ? -a : a;
        }
        return gcd(b, a % b);
    }

    public static void main(String[] args) throws FileNotFoundException {
        Scanner scanner = new Scanner(new File("input.txt"));
        List<String> grid = new ArrayList<>();
        while (scanner.hasNextLine()) {
            grid.add(scanner.nextLine());
        }
        scanner.close();

        int h = grid.size();
        int w = grid.get(0).length();
        Map<Character, List<int[]>> antennas = new HashMap<>();
        for (int y = 0; y < h; y++) {
            for (int x = 0; x < w; x++) {
                char c = grid.get(y).charAt(x);
                if (c != '.') {
                    antennas.computeIfAbsent(c, k -> new ArrayList<>()).add(new int[]{y, x});
                }
            }
        }

        Map<Character, Set<String>> linesPerFreq = new HashMap<>();
        for (Map.Entry<Character, List<int[]>> entry : antennas.entrySet()) {
            char f = entry.getKey();
            linesPerFreq.put(f, new HashSet<>());
            List<int[]> coords = entry.getValue();
            int n = coords.size();
            for (int i = 0; i < n; i++) {
                for (int j = i + 1; j < n; j++) {
                    int[] A = coords.get(i);
                    int[] B = coords.get(j);
                    int dy = B[0] - A[0];
                    int dx = B[1] - A[1];
                    int g = gcd(dy, dx);
                    int sy = dy / g;
                    int sx = dx / g;
                    if (sx < 0 || (sx == 0 && sy < 0)) {
                        sx = -sx;
                        sy = -sy;
                    }
                    int c = sy * A[1] - sx * A[0];
                    linesPerFreq.get(f).add(sx + "," + sy + "," + c);
                }
            }
        }

        Set<String> antinodes = new HashSet<>();
        for (Set<String> lines : linesPerFreq.values()) {
            for (String key : lines) {
                String[] parts = key.split(",");
                int sx = Integer.parseInt(parts[0]);
                int sy = Integer.parseInt(parts[1]);
                int c = Integer.parseInt(parts[2]);
                if (sx == 0 && sy == 0) continue;
                if (sy == 0) {
                    if (c % sx == 0) {
                        int y = -c / sx;
                        if (y >= 0 && y < h) {
                            for (int x = 0; x < w; x++) {
                                antinodes.add(y + "," + x);
                            }
                        }
                    }
                } else if (sx == 0) {
                    if (c % sy == 0) {
                        int x = c / sy;
                        if (x >= 0 && x < w) {
                            for (int y = 0; y < h; y++) {
                                antinodes.add(y + "," + x);
                            }
                        }
                    }
                } else {
                    for (int y = 0; y < h; y++) {
                        int val = c + sx * y;
                        if (val % sy == 0) {
                            int x = val / sy;
                            if (x >= 0 && x < w) {
                                antinodes.add(y + "," + x);
                            }
                        }
                    }
                }
            }
        }
        System.out.println(antinodes.size());
    }
}
