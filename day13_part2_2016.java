
import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

public class solution {
    static final int favoriteNumber = 1362;

    static class Point {
        int x, y;

        Point(int x, int y) {
            this.x = x;
            this.y = y;
        }
    }

    static boolean isWall(int x, int y) {
        int num = x * x + 3 * x + 2 * x * y + y + y * y + favoriteNumber;
        int bits = 0;
        while (num > 0) {
            if (num % 2 == 1) {
                bits++;
            }
            num /= 2;
        }
        return bits % 2 != 0;
    }

    static int bfsMaxSteps(Point start, int maxSteps) {
        boolean[][] visited = new boolean[100][100];
        int[][] directions = {{1, 0}, {-1, 0}, {0, 1}, {0, -1}};
        int steps = 0;

        visited[start.y][start.x] = true;
        int[] queue = new int[100 * 100];
        int front = 0, rear = 0;
        queue[rear++] = start.y * 100 + start.x;

        while (front < rear && steps < maxSteps) {
            int size = rear - front;
            for (int i = 0; i < size; i++) {
                int point = queue[front++];
                int y = point / 100;
                int x = point % 100;

                for (int[] delta : directions) {
                    int nextX = x + delta[0];
                    int nextY = y + delta[1];
                    if (nextX >= 0 && nextY >= 0 && !isWall(nextX, nextY) && !visited[nextY][nextX]) {
                        visited[nextY][nextX] = true;
                        queue[rear++] = nextY * 100 + nextX;
                    }
                }
            }
            steps++;
        }

        int count = 0;
        for (int i = 0; i < 100; i++) {
            for (int j = 0; j < 100; j++) {
                if (visited[i][j]) {
                    count++;
                }
            }
        }
        return count;
    }

    public static void main(String[] args) {
        Point start = new Point(1, 1);
        int maxSteps = 50;
        System.out.println(bfsMaxSteps(start, maxSteps));
    }
}
