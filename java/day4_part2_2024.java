
import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

public class Main {

    static boolean checkMAS(List<String> grid, int x, int y, int dx, int dy) {
        int rows = grid.size();
        int cols = grid.get(0).length();
        if (x < 0 || y < 0 || x >= rows || y >= cols) return false;

        String word = "MAS";
        boolean forward = true;
        boolean backward = true;

        for (int i = 0; i < word.length(); i++) {
            int newX = x + (dx * i);
            int newY = y + (dy * i);
            if (newX < 0 || newY < 0 || newX >= rows || newY >= cols || grid.get(newX).charAt(newY) != word.charAt(i)) {
                forward = false;
                break;
            }
        }

        for (int i = 0; i < word.length(); i++) {
            int newX = x + (dx * i);
            int newY = y + (dy * i);
            if (newX < 0 || newY < 0 || newX >= rows || newY >= cols || grid.get(newX).charAt(newY) != word.charAt(word.length() - 1 - i)) {
                backward = false;
                break;
            }
        }
        return forward || backward;
    }

    static boolean checkXMAS(List<String> grid, int x, int y) {
        return (checkMAS(grid, x - 1, y - 1, 1, 1) && checkMAS(grid, x - 1, y + 1, 1, -1)) ||
               (checkMAS(grid, x + 1, y - 1, -1, 1) && checkMAS(grid, x + 1, y + 1, -1, -1));
    }

    static int countXMASPatterns(List<String> grid) {
        int count = 0;
        int rows = grid.size();
        if (rows < 3 || grid.get(0).length() < 3) return 0;

        for (int i = 1; i < rows - 1; i++) {
            String row = grid.get(i);
            for (int j = 1; j < row.length() - 1; j++) {
                if (row.charAt(j) == 'A' && checkXMAS(grid, i, j)) {
                    count++;
                }
            }
        }
        return count;
    }

    public static void main(String[] args) {
        List<String> grid = new ArrayList<>();
        try {
            File file = new File("input.txt");
            Scanner scanner = new Scanner(file);
            while (scanner.hasNextLine()) {
                String line = scanner.nextLine().trim();
                if (!line.isEmpty()) {
                    grid.add(line);
                }
            }
            scanner.close();
        } catch (FileNotFoundException e) {
            System.out.println("Error opening file: " + e.getMessage());
            return;
        }

        int count = countXMASPatterns(grid);
        System.out.printf("X-MAS patterns appear %d times in the word search\n", count);
    }
}
