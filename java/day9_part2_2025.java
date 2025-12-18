
import java.io.*;
import java.util.*;

/**
 * Day 9: Movie Theater - Solution
 * 
 * This program solves the challenge of finding the largest rectangle formed by
 * two red tiles as opposite corners, such that the entire rectangle consists
 * only of red or green tiles. The red tiles form a loop, and the green tiles
 * comprise the loop's connections and its entire interior.
 */
public class Solution {

    static class Point {
        long x, y;

        Point(long x, long y) {
            this.x = x;
            this.y = y;
        }
    }

    public static void main(String[] args) {
        List<Point> redTiles = new ArrayList<>();

        // Reading the input from input.txt
        try (BufferedReader br = new BufferedReader(new FileReader("input.txt"))) {
            String line;
            while ((line = br.readLine()) != null) {
                line = line.trim();
                if (line.isEmpty()) continue;
                String[] parts = line.split(",");
                if (parts.length >= 2) {
                    long x = Long.parseLong(parts[0].trim());
                    long y = Long.parseLong(parts[1].trim());
                    redTiles.add(new Point(x, y));
                }
            }
        } catch (IOException | NumberFormatException e) {
            // Silently handle exceptions as per challenge requirements for a concise solution
            return;
        }

        if (redTiles.isEmpty()) return;

        // --- PART 2 LOGIC ---
        // A rectangle is valid if it lies entirely within the rectilinear polygon 
        // formed by the red tiles loop.

        // Step 1: Collect unique coordinates to create a coordinate-compressed grid
        TreeSet<Long> uniqueX = new TreeSet<>();
        TreeSet<Long> uniqueY = new TreeSet<>();
        for (Point p : redTiles) {
            uniqueX.add(p.x);
            uniqueY.add(p.y);
        }

        Long[] sortedX = uniqueX.toArray(new Long[0]);
        Long[] sortedY = uniqueY.toArray(new Long[0]);
        Map<Long, Integer> xMap = new HashMap<>();
        Map<Long, Integer> yMap = new HashMap<>();
        for (int i = 0; i < sortedX.length; i++) xMap.put(sortedX[i], i);
        for (int j = 0; j < sortedY.length; j++) yMap.put(sortedY[j], j);

        // Grid sizing for "half-integer" grid to represent vertices, edges, and cells
        int gridW = 2 * sortedX.length - 1;
        int gridH = 2 * sortedY.length - 1;
        boolean[][] grid = new boolean[gridW][gridH];

        // Step 2: Mark boundaries of the polygon (red and green connections)
        for (int i = 0; i < redTiles.size(); i++) {
            Point p1 = redTiles.get(i);
            Point p2 = redTiles.get((i + 1) % redTiles.size());
            int ix1 = xMap.get(p1.x), iy1 = yMap.get(p1.y);
            int ix2 = xMap.get(p2.x), iy2 = yMap.get(p2.y);
            // Mark all indices in grid between p1 and p2 as valid (on boundary)
            for (int x = 2 * Math.min(ix1, ix2); x <= 2 * Math.max(ix1, ix2); x++) {
                for (int y = 2 * Math.min(iy1, iy2); y <= 2 * Math.max(iy1, iy2); y++) {
                    grid[x][y] = true;
                }
            }
        }

        // Step 3: Mark interior cells using a scanline Point-in-Polygon approach
        for (int i = 0; i < sortedX.length - 1; i++) {
            double midX = (sortedX[i] + sortedX[i + 1]) / 2.0;
            List<Long> crossingY = new ArrayList<>();
            for (int k = 0; k < redTiles.size(); k++) {
                Point p1 = redTiles.get(k);
                Point p2 = redTiles.get((k + 1) % redTiles.size());
                // Detect horizontal segments crossing the current vertical slab
                if (p1.y == p2.y && ((p1.x < midX && midX < p2.x) || (p2.x < midX && midX < p1.x))) {
                    crossingY.add(p1.y);
                }
            }
            Collections.sort(crossingY);
            boolean inside = false;
            int crossingIdx = 0;
            for (int j = 0; j < sortedY.length - 1; j++) {
                double midY = (sortedY[j] + sortedY[j + 1]) / 2.0;
                while (crossingIdx < crossingY.size() && crossingY.get(crossingIdx) < midY) {
                    inside = !inside;
                    crossingIdx++;
                }
                if (inside) {
                    // Mark the 3x3 grid area for this interior cell
                    for (int gx = 2 * i; gx <= 2 * i + 2; gx++) {
                        for (int gy = 2 * j; gy <= 2 * j + 2; gy++) {
                            grid[gx][gy] = true;
                        }
                    }
                }
            }
        }

        // Step 4: Use 2D Prefix Sums to verify rectangle validity in O(1)
        int[][] pref = new int[gridW + 1][gridH + 1];
        for (int i = 0; i < gridW; i++) {
            for (int j = 0; j < gridH; j++) {
                pref[i + 1][j + 1] = (grid[i][j] ? 1 : 0) + pref[i][j + 1] + pref[i + 1][j] - pref[i][j];
            }
        }

        long maxAreaPart2 = 0;
        // Step 5: Evaluate every pair of red tiles as rectangle corners
        for (int i = 0; i < redTiles.size(); i++) {
            for (int j = 0; j < redTiles.size(); j++) {
                Point p1 = redTiles.get(i);
                Point p2 = redTiles.get(j);
                
                int gx1 = 2 * xMap.get(p1.x), gy1 = 2 * yMap.get(p1.y);
                int gx2 = 2 * xMap.get(p2.x), gy2 = 2 * yMap.get(p2.y);
                int minGX = Math.min(gx1, gx2), maxGX = Math.max(gx1, gx2);
                int minGY = Math.min(gy1, gy2), maxGY = Math.max(gy1, gy2);

                long requiredCount = (long) (maxGX - minGX + 1) * (maxGY - minGY + 1);
                int actualCount = pref[maxGX + 1][maxGY + 1] - pref[minGX][maxGY + 1] 
                                - pref[maxGX + 1][minGY] + pref[minGX][minGY];
                
                // If every point in the rectangle is red or green (grid[x][y] == true)
                if (actualCount == requiredCount) {
                    long area = (Math.abs(p1.x - p2.x) + 1) * (Math.abs(p1.y - p2.y) + 1);
                    if (area > maxAreaPart2) maxAreaPart2 = area;
                }
            }
        }

        // Output the result of Part 2 to standard output
        System.out.println(maxAreaPart2);
    }
}

