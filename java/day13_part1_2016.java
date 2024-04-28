import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.LinkedList;
import java.util.Queue;

public class MazeSolver {

    public static void main(String[] args) {
        try (BufferedReader br = new BufferedReader(new FileReader("input.txt"))) {
            String input = br.readLine();
            int favoriteNumber = Integer.parseInt(input);
            System.out.println("Fewest steps required to reach 31,39: " + findFewestSteps(favoriteNumber, 31, 39));
        } catch (IOException e) {
            System.err.println("Error reading input file: " + e.getMessage());
        }
    }

    private static int findFewestSteps(int favoriteNumber, int targetX, int targetY) {
        boolean[][] maze = new boolean[100][100]; // Assuming the maze is at most 100x100
        for (int x = 0; x < 100; x++) {
            for (int y = 0; y < 100; y++) {
                int formula = x * x + 3 * x + 2 * x * y + y + y * y + favoriteNumber;
                String binary = Integer.toBinaryString(formula);
                int ones = countOnes(binary);
                maze[x][y] = ones % 2 == 0; // Open space if even, wall if odd
            }
        }

        boolean[][] visited = new boolean[100][100];
        int[][] directions = {{-1, 0}, {1, 0}, {0, -1}, {0, 1}};
        int steps = 0;
        Queue<Integer> queue = new LinkedList<>();
        queue.add(1);
        queue.add(1);
        queue.add(0);

        while (!queue.isEmpty()) {
            int x = queue.poll();
            int y = queue.poll();
            int step = queue.poll();

            if (x == targetX && y == targetY) {
                return step;
            }

            for (int[] direction : directions) {
                int newX = x + direction[0];
                int newY = y + direction[1];

                if (newX >= 0 && newX < 100 && newY >= 0 && newY < 100 && maze[newX][newY] && !visited[newX][newY]) {
                    queue.add(newX);
                    queue.add(newY);
                    queue.add(step + 1);
                    visited[newX][newY] = true;
                }
            }
        }

        return -1; // Target not reachable
    }

    private static int countOnes(String binary) {
        int count = 0;
        for (char c : binary.toCharArray()) {
            if (c == '1') {
                count++;
            }
        }
        return count;
    }
}