
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

/**
 * Movie Theater Challenge
 * 
 * The goal is to find the largest axis-aligned rectangle where any two red tiles
 * from the input list serve as opposite corners.
 * Area formula: (abs(x1 - x2) + 1) * (abs(y1 - y2) + 1)
 */
public class MovieTheater {

    public static void main(String[] args) {
        String fileName = "input.txt";
        List<Integer> xCoords = new ArrayList<>();
        List<Integer> yCoords = new ArrayList<>();

        // Efficiently read coordinates from input.txt
        try (BufferedReader br = new BufferedReader(new FileReader(fileName))) {
            String line;
            while ((line = br.readLine()) != null) {
                line = line.trim();
                if (line.isEmpty()) continue;

                int commaIndex = line.indexOf(',');
                if (commaIndex != -1) {
                    try {
                        int x = Integer.parseInt(line.substring(0, commaIndex).trim());
                        int y = Integer.parseInt(line.substring(commaIndex + 1).trim());
                        xCoords.add(x);
                        yCoords.add(y);
                    } catch (NumberFormatException e) {
                        // Skip lines that are not valid integer pairs
                    }
                }
            }
        } catch (IOException e) {
            // Handle file not found or read errors
            return;
        }

        int n = xCoords.size();
        if (n == 0) return;

        // Convert to primitive arrays to optimize memory access and CPU cache usage
        int[] x = new int[n];
        int[] y = new int[n];
        for (int i = 0; i < n; i++) {
            x[i] = xCoords.get(i);
            y[i] = yCoords.get(i);
        }

        long maxArea = 0;

        // If at least one tile exists, the minimum possible area (1x1) is 1
        if (n > 0) maxArea = 1;

        /*
         * O(N^2) search: Compare every pair of red tiles.
         * For N up to ~15,000, this completes in well under a second in Java.
         * The problem asks for the area of a rectangle using two tiles as opposite corners.
         */
        for (int i = 0; i < n; i++) {
            int x1 = x[i];
            int y1 = y[i];
            for (int j = i + 1; j < n; j++) {
                // Width = horizontal distance + 1; Height = vertical distance + 1
                long width = Math.abs(x1 - x[j]) + 1;
                long height = Math.abs(y1 - y[j]) + 1;
                
                long area = width * height;
                if (area > maxArea) {
                    maxArea = area;
                }
            }
        }

        // Output the result to standard output
        System.out.println(maxArea);
    }
}
