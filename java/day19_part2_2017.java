
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class Main {
    public static void main(String[] args) throws IOException {
        BufferedReader reader = new BufferedReader(new FileReader("input.txt"));
        StringBuilder gridBuilder = new StringBuilder();
        String line;
        while ((line = reader.readLine()) != null) {
            gridBuilder.append(line).append("\n");
        }
        reader.close();
        
        String[] grid = gridBuilder.toString().split("\n");
        int x = 0, y = 0;
        for (int i = 0; i < grid[0].length(); i++) {
            if (grid[0].charAt(i) == '|') {
                x = i;
                break;
            }
        }

        int dx = 0, dy = 1, steps = 0;

        while (true) {
            if (x < 0 || x >= grid[0].length() || y < 0 || y >= grid.length) break;

            char cell = grid[y].charAt(x);
            if (cell == ' ') break;

            steps++;

            if (cell == '+') {
                if (dx == 0) {
                    dx = (x > 0 && (grid[y].charAt(x - 1) == '-' || Character.isUpperCase(grid[y].charAt(x - 1)))) ? -1 : 1;
                    dy = 0;
                } else {
                    dx = 0;
                    dy = (y > 0 && (grid[y - 1].charAt(x) == '|' || Character.isUpperCase(grid[y - 1].charAt(x)))) ? -1 : 1;
                }
            }

            x += dx;
            y += dy;
        }

        System.out.println(steps);
    }
}
