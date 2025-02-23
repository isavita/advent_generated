
import java.io.File;
import java.io.IOException;
import java.util.LinkedList;
import java.util.Queue;
import java.util.Scanner;

public class Solution {

  static int rows, cols;
  static char[][] grid;
  static boolean[][] visited;

  public static void main(String[] args) throws IOException {
    Scanner scanner = new Scanner(new File("input.txt"));
    rows = 0;
    String firstLine = scanner.nextLine();
    cols = firstLine.length();
    rows++;
    while (scanner.hasNextLine()) {
      scanner.nextLine();
      rows++;
    }
    scanner.close();

    grid = new char[rows][cols];
    visited = new boolean[rows][cols];

    scanner = new Scanner(new File("input.txt"));
    for (int i = 0; i < rows; i++) {
      String line = scanner.nextLine();
      for (int j = 0; j < cols; j++) {
        grid[i][j] = line.charAt(j);
      }
    }
    scanner.close();

    long totalPrice = 0;
    for (int r = 0; r < rows; r++) {
      for (int c = 0; c < cols; c++) {
        if (!visited[r][c]) {
          long[] result = calculateRegion(r, c);
          totalPrice += result[0] * result[1];
        }
      }
    }
    System.out.println(totalPrice);
  }

  static long[] calculateRegion(int row, int col) {
    char character = grid[row][col];
    long area = 0;
    long perimeter = 0;
    Queue<int[]> queue = new LinkedList<>();
    queue.offer(new int[] {row, col});
    visited[row][col] = true;

    while (!queue.isEmpty()) {
      int[] current = queue.poll();
      int r = current[0];
      int c = current[1];
      area++;

      boolean isBorder = r == 0 || r == rows - 1 || c == 0 || c == cols - 1;

      int[][] directions = {{0, 1}, {0, -1}, {1, 0}, {-1, 0}};
      for (int[] dir : directions) {
        int nr = r + dir[0];
        int nc = c + dir[1];
        if (nr >= 0 && nr < rows && nc >= 0 && nc < cols) {
          if (grid[nr][nc] != character) {
            perimeter++;
          } else if (!visited[nr][nc]) {
            visited[nr][nc] = true;
            queue.offer(new int[] {nr, nc});
          }
        } else if (isBorder) {
          perimeter++;
        }
      }
    }

    return new long[] {area, perimeter};
  }
}
