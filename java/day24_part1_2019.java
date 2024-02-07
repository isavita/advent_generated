
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;

public class solution {
    static final int Side = 5;
    static final int Square = Side * Side;

    public static void main(String[] args) {
        HashMap<Integer, Boolean> appeared = new HashMap<>();

        boolean[] grid = parse();
        appeared.put(biodiversity(grid), true);
        while (true) {
            grid = next1(grid);
            if (appeared.containsKey(biodiversity(grid))) {
                System.out.println(biodiversity(grid));
                return;
            }
            appeared.put(biodiversity(grid), true);
        }
    }

    public static boolean[] parse() {
        boolean[] res = new boolean[Square];

        try (BufferedReader br = new BufferedReader(new FileReader("input.txt"))) {
            String line;
            int row = 0;
            while ((line = br.readLine()) != null) {
                for (int col = 0; col < Side; col++) {
                    if (line.charAt(col) == '#') {
                        res[row * Side + col] = true;
                    } else {
                        res[row * Side + col] = false;
                    }
                }
                row++;
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
        return res;
    }

    public static boolean[] next1(boolean[] grid) {
        boolean[] newGrid = new boolean[Square];

        for (int i = 0; i < Square; i++) {
            int row = i / Side;
            int col = i % Side;
            int neighbours = 0;

            if (row > 0 && grid[i - Side]) {
                neighbours++;
            }
            if (row < Side - 1 && grid[i + Side]) {
                neighbours++;
            }
            if (col > 0 && grid[i - 1]) {
                neighbours++;
            }
            if (col < Side - 1 && grid[i + 1]) {
                neighbours++;
            }

            if (grid[i] && neighbours != 1) {
                newGrid[i] = false;
                continue;
            }

            if (!grid[i] && (neighbours == 1 || neighbours == 2)) {
                newGrid[i] = true;
                continue;
            }

            newGrid[i] = grid[i];
        }

        return newGrid;
    }

    public static int biodiversity(boolean[] grid) {
        int bio = 0;
        for (int i = 0; i < Square; i++) {
            if (grid[i]) {
                bio += 1 << i;
            }
        }
        return bio;
    }
}
