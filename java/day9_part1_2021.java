import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class SmokeBasin {

    public static void main(String[] args) {
        try (BufferedReader br = new BufferedReader(new FileReader("input.txt"))) {
            String line;
            int sum = 0;
            int[][] heights = new int[100][100]; // assuming the input is not larger than 100x100
            int row = 0;
            while ((line = br.readLine()) != null) {
                for (int col = 0; col < line.length(); col++) {
                    heights[row][col] = line.charAt(col) - '0';
                }
                row++;
            }

            int cols = heights[0].length; // store the number of columns
            for (int i = 0; i < row; i++) {
                for (int j = 0; j < cols; j++) {
                    if (isLowPoint(heights, i, j)) {
                        sum += heights[i][j] + 1;
                    }
                }
            }

            System.out.println("The sum of the risk levels of all low points is: " + sum);
        } catch (IOException e) {
            System.err.println("Error reading file: " + e.getMessage());
        }
    }

    private static boolean isLowPoint(int[][] heights, int i, int j) {
        int height = heights[i][j];
        if (i > 0 && heights[i - 1][j] <= height) return false;
        if (i < heights.length - 1 && heights[i + 1][j] <= height) return false;
        if (j > 0 && heights[i][j - 1] <= height) return false;
        if (j < heights[0].length - 1 && heights[i][j + 1] <= height) return false;
        return true;
    }
}