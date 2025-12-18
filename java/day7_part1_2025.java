
import java.util.*;
import java.io.*;

/**
 * Solution for the Tachyon Manifold splitter problem.
 * 
 * The program simulates the path of tachyon beams as they move downward through a manifold.
 * Beams enter at location 'S' and travel vertically down. When a beam encounters a 
 * splitter (^), it increments the split count, stops, and produces two new beams 
 * at the immediate left and right of the splitter for the next row.
 * 
 * The program reads from 'input.txt' and prints the total number of splits to stdout.
 */
public class Solution {
    public static void main(String[] args) {
        List<String> grid = new ArrayList<>();
        
        // Load the manifold grid from input.txt
        try {
            File inputFile = new File("input.txt");
            if (!inputFile.exists()) {
                return;
            }
            try (Scanner sc = new Scanner(inputFile)) {
                while (sc.hasNextLine()) {
                    grid.add(sc.nextLine());
                }
            }
        } catch (IOException e) {
            // Handle IO exceptions gracefully
            return;
        }

        // Find the coordinates of the entry point 'S'
        int startRow = -1;
        int startCol = -1;
        for (int r = 0; r < grid.size(); r++) {
            String row = grid.get(r);
            int c = row.indexOf('S');
            if (c != -1) {
                startRow = r;
                startCol = c;
                break;
            }
        }

        // If no entry point 'S' is found, there are no beams to simulate
        if (startRow == -1) {
            System.out.println(0);
            return;
        }

        // Maintain a Set of column indices where beams are currently located.
        // A Set automatically handles the merging of tachyon beams: 
        // if two beams enter the same column at the same time, they continue as one.
        Set<Integer> currentBeams = new HashSet<>();
        currentBeams.add(startCol);
        
        long totalSplits = 0;

        // Process each row sequentially, starting from the row immediately below 'S'.
        // This simulates the beams moving downward row by row.
        for (int r = startRow + 1; r < grid.size(); r++) {
            Set<Integer> nextBeams = new HashSet<>();
            String rowStr = grid.get(r);
            
            for (int c : currentBeams) {
                // Check if the current beam column is within the grid boundaries
                if (c >= 0 && c < rowStr.length()) {
                    char cell = rowStr.charAt(c);
                    if (cell == '^') {
                        // The beam hits a splitter
                        totalSplits++;
                        
                        // Split the beam: new beams continue from left and right of the splitter.
                        // These new beams will be processed for the next row.
                        nextBeams.add(c - 1);
                        nextBeams.add(c + 1);
                    } else {
                        // Beam passes through empty space or another character
                        nextBeams.add(c);
                    }
                }
                // If the beam goes out of bounds horizontally, it simply exits the manifold.
            }
            
            // Prepare for the next row
            currentBeams = nextBeams;
            
            // Optimization: if no beams are currently active, stop processing the grid
            if (currentBeams.isEmpty()) {
                break;
            }
        }

        // Print the final count of splits to standard output
        System.out.println(totalSplits);
    }
}

