
import java.io.File;
import java.io.FileNotFoundException;
import java.util.HashMap;
import java.util.Map;
import java.util.Scanner;

public class CartPath {

    public static void main(String[] args) throws FileNotFoundException {
        Scanner scanner = new Scanner(new File("input.txt"));
        char[][] grid = new char[150][150]; // Adjust size as needed
        int rows = 0;
        while (scanner.hasNextLine()) {
            String line = scanner.nextLine();
            grid[rows] = line.toCharArray();
            rows++;
        }
        scanner.close();

        int cols = grid[0].length;
        int startX = -1, startY = -1;
        int dirX = 0, dirY = 0;
        int dirIdx = -1;

        for (int i = 0; i < rows; i++) {
            for (int j = 0; j < cols; j++) {
                switch (grid[i][j]) {
                    case '^':
                        startX = j;
                        startY = i;
                        dirIdx = 0;
                        dirX = 0;
                        dirY = -1;
                        break;
                    case '>':
                        startX = j;
                        startY = i;
                        dirIdx = 1;
                        dirX = 1;
                        dirY = 0;
                        break;
                    case 'v':
                        startX = j;
                        startY = i;
                        dirIdx = 2;
                        dirX = 0;
                        dirY = 1;
                        break;
                    case '<':
                        startX = j;
                        startY = i;
                        dirIdx = 3;
                        dirX = -1;
                        dirY = 0;
                        break;
                }
                if (startX != -1) break;
            }
            if (startX != -1) break;
        }


        Map<String, Boolean> visited = new HashMap<>();
        visited.put(startX + "," + startY, true);
        int x = startX, y = startY;

        while (true) {
            int nx = x + dirX;
            int ny = y + dirY;

            if (nx < 0 || nx >= cols || ny < 0 || ny >= rows) break;

            if (grid[ny][nx] == '#') {
                dirIdx = (dirIdx + 1) % 4;
                switch (dirIdx) {
                    case 0:
                        dirX = 0;
                        dirY = -1;
                        break;
                    case 1:
                        dirX = 1;
                        dirY = 0;
                        break;
                    case 2:
                        dirX = 0;
                        dirY = 1;
                        break;
                    case 3:
                        dirX = -1;
                        dirY = 0;
                        break;
                }
                continue;
            }
            x = nx;
            y = ny;
            visited.put(x + "," + y, true);
        }
        System.out.println(visited.size());
    }
}
