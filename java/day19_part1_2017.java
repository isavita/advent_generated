import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class Main {
    public static void main(String[] args) throws IOException {
        try (BufferedReader br = new BufferedReader(new FileReader("input.txt"))) {
            String line;
            StringBuilder sb = new StringBuilder();
            int x = 0, y = 0;
            String[] grid = new String[1000]; // assuming max 1000 lines
            int i = 0;
            while ((line = br.readLine()) != null) {
                grid[i++] = line;
                if (x == 0) {
                    for (int j = 0; j < line.length(); j++) {
                        if (line.charAt(j) == '|') {
                            x = j;
                            break;
                        }
                    }
                }
            }

            int dx = 0, dy = 1;
            StringBuilder letters = new StringBuilder();

            while (true) {
                if (x < 0 || x >= grid[0].length() || y < 0 || y >= grid.length) {
                    break;
                }

                char cell = grid[y].charAt(x);

                if (cell == ' ') {
                    break;
                }

                if (cell >= 'A' && cell <= 'Z') {
                    letters.append(cell);
                }

                if (cell == '+') {
                    if (dx == 0) {
                        if (x > 0 && (grid[y].charAt(x - 1) == '-' || (grid[y].charAt(x - 1) >= 'A' && grid[y].charAt(x - 1) <= 'Z'))) {
                            dx = -1;
                            dy = 0;
                        } else {
                            dx = 1;
                            dy = 0;
                        }
                    } else {
                        if (y > 0 && (grid[y - 1].charAt(x) == '|' || (grid[y - 1].charAt(x) >= 'A' && grid[y - 1].charAt(x) <= 'Z'))) {
                            dx = 0;
                            dy = -1;
                        } else {
                            dx = 0;
                            dy = 1;
                        }
                    }
                }

                x += dx;
                y += dy;
            }

            System.out.println(letters.toString());
        }
    }
}