import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.Arrays;
import java.util.StringTokenizer;

public class Main {
    static class Point {
        int x, y;
    }

    public static void main(String[] args) throws IOException {
        BufferedReader br = new BufferedReader(new FileReader("input.txt"));
        String line;
        int maxX = 0, maxY = 0;
        int n = 0;
        while ((line = br.readLine()) != null) {
            StringTokenizer st = new StringTokenizer(line, ", ");
            int x = Integer.parseInt(st.nextToken());
            int y = Integer.parseInt(st.nextToken());
            if (x > maxX) maxX = x;
            if (y > maxY) maxY = y;
            n++;
        }
        br.close();
        Point[] points = new Point[n];
        br = new BufferedReader(new FileReader("input.txt"));
        int index = 0;
        while ((line = br.readLine()) != null) {
            StringTokenizer st = new StringTokenizer(line, ", ");
            points[index] = new Point();
            points[index].x = Integer.parseInt(st.nextToken());
            points[index].y = Integer.parseInt(st.nextToken());
            index++;
        }
        br.close();
        int[][] grid = new int[maxX + 2][maxY + 2];
        int[] areas = new int[n];
        boolean[] infinite = new boolean[n];
        for (int x = 0; x <= maxX + 1; x++) {
            for (int y = 0; y <= maxY + 1; y++) {
                int minDist = maxX + maxY;
                int minPoint = -1;
                for (int k = 0; k < n; k++) {
                    int dist = Math.abs(points[k].x - x) + Math.abs(points[k].y - y);
                    if (dist < minDist) {
                        minDist = dist;
                        minPoint = k;
                    } else if (dist == minDist) {
                        minPoint = -1;
                    }
                }
                grid[x][y] = minPoint;
                if (minPoint != -1) {
                    if (x == 0 || y == 0 || x == maxX + 1 || y == maxY + 1) {
                        infinite[minPoint] = true;
                    }
                    areas[minPoint]++;
                }
            }
        }
        int maxArea = 0;
        for (int j = 0; j < n; j++) {
            if (!infinite[j] && areas[j] > maxArea) {
                maxArea = areas[j];
            }
        }
        System.out.println(maxArea);
    }
}