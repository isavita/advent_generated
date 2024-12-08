
import java.io.File;
import java.io.FileNotFoundException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Scanner;
import java.util.Set;

public class Antennas {

    public static void main(String[] args) throws FileNotFoundException {
        Scanner scanner = new Scanner(new File("input.txt"));
        String[] grid = new String[1000]; // Adjust size as needed.  Pre-allocate for efficiency.
        int h = 0;
        while (scanner.hasNextLine()) {
            grid[h++] = scanner.nextLine();
        }
        scanner.close();

        int w = grid[0].length();
        Map<Character, java.util.List<int[]>> antennas = new HashMap<>();
        for (int y = 0; y < h; y++) {
            for (int x = 0; x < w; x++) {
                char c = grid[y].charAt(x);
                if (c != '.') {
                    antennas.computeIfAbsent(c, k -> new java.util.ArrayList<>()).add(new int[]{y, x});
                }
            }
        }

        Set<String> antinodes = new HashSet<>();
        for (java.util.List<int[]> coords : antennas.values()) {
            int n = coords.size();
            for (int i = 0; i < n; i++) {
                for (int j = i + 1; j < n; j++) {
                    int[] A = coords.get(i);
                    int[] B = coords.get(j);
                    int[] P1 = {2 * A[0] - B[0], 2 * A[1] - B[1]};
                    int[] P2 = {2 * B[0] - A[0], 2 * B[1] - A[1]};
                    if (P1[0] >= 0 && P1[0] < h && P1[1] >= 0 && P1[1] < w) {
                        antinodes.add(P1[0] + "," + P1[1]);
                    }
                    if (P2[0] >= 0 && P2[0] < h && P2[1] >= 0 && P2[1] < w) {
                        antinodes.add(P2[0] + "," + P2[1]);
                    }
                }
            }
        }
        System.out.println(antinodes.size());
    }
}
