
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

class Solution {

    static int[][] directions = {{0, 1}, {1, 0}, {1, 1}, {-1, 1}, {0, -1}, {-1, 0}, {-1, -1}, {1, -1}};

    static boolean checkWord(char[][] grid, String word, int x, int y, int dx, int dy) {
        int rows = grid.length;
        int cols = grid[0].length;
        int wordLen = word.length();

        for (int i = 0; i < wordLen; i++) {
            int newX = x + dx * i;
            int newY = y + dy * i;
            if (newX < 0 || newY < 0 || newX >= rows || newY >= cols || grid[newX][newY] != word.charAt(i)) {
                return false;
            }
        }
        return true;
    }

    static int countOccurrences(char[][] grid, String word) {
        int count = 0;
        int rows = grid.length;
        int cols = grid[0].length;

        for (int i = 0; i < rows; i++) {
            for (int j = 0; j < cols; j++) {
                for (int[] dir : directions) {
                    if (checkWord(grid, word, i, j, dir[0], dir[1])) {
                        count++;
                    }
                }
            }
        }
        return count;
    }

    public static void main(String[] args) {
        List<String> lines = new ArrayList<>();
        try (BufferedReader br = new BufferedReader(new FileReader("input.txt"))) {
            String line;
            while ((line = br.readLine()) != null) {
                line = line.trim();
                if (!line.isEmpty()) {
                    lines.add(line);
                }
            }
        } catch (IOException e) {
            System.out.println("Error reading file: " + e.getMessage());
            return;
        }

        int rows = lines.size();
        if (rows == 0) {
            System.out.println("XMAS appears 0 times in the word search");
            return;
        }
        int cols = lines.get(0).length();
        char[][] grid = new char[rows][cols];
        for (int i = 0; i < rows; i++) {
            grid[i] = lines.get(i).toCharArray();
        }

        int count = countOccurrences(grid, "XMAS");
        System.out.printf("XMAS appears %d times in the word search\n", count);
    }
}
