
import java.io.*;
import java.util.*;
import java.util.regex.*;

/**
 * Solution for the Factory initialization challenge.
 * 
 * This program calculates the minimum total button presses required to configure
 * indicator lights for multiple machines. Each machine's configuration is solved
 * as a system of linear equations over GF(2) (the field of two elements, 0 and 1).
 * 
 * The light states are modeled as a vector 'b', and each button press is modeled 
 * as a column in a matrix 'A'. Solving Ax = b over GF(2) gives us the required
 * button presses. Gaussian elimination is used to simplify the system, and
 * free variables are brute-forced to find the solution with the minimum Hamming weight
 * (fewest total presses).
 */
public class Solution {
    public static void main(String[] args) {
        long totalMinPresses = 0;
        File inputFile = new File("input.txt");
        
        if (!inputFile.exists()) {
            return;
        }

        try (BufferedReader reader = new BufferedReader(new FileReader(inputFile))) {
            String line;
            while ((line = reader.readLine()) != null) {
                line = line.trim();
                if (line.isEmpty()) continue;
                totalMinPresses += solveMachine(line);
            }
            // Output the total fewest presses for all machines to standard output
            System.out.println(totalMinPresses);
        } catch (IOException e) {
            // Error handling for file reading
        }
    }

    /**
     * Solves the minimum button presses for a single machine line.
     */
    private static long solveMachine(String line) {
        // 1. Parse the target indicator light configuration from [brackets]
        int startBracket = line.indexOf('[');
        int endBracket = line.indexOf(']');
        if (startBracket == -1 || endBracket == -1) return 0;
        
        String targetConfig = line.substring(startBracket + 1, endBracket);
        int numLights = targetConfig.length();
        int[] targetVec = new int[numLights];
        for (int i = 0; i < numLights; i++) {
            targetVec[i] = (targetConfig.charAt(i) == '#') ? 1 : 0;
        }

        // 2. Parse button wiring schematics from (parentheses)
        List<int[]> buttons = new ArrayList<>();
        Matcher m = Pattern.compile("\\((.*?)\\)").matcher(line);
        while (m.find()) {
            String[] indices = m.group(1).split(",");
            int[] buttonVec = new int[numLights];
            for (String idxStr : indices) {
                String trimmed = idxStr.trim();
                if (trimmed.isEmpty()) continue;
                int lightIdx = Integer.parseInt(trimmed);
                // Buttons only affect lights within the diagram's range
                if (lightIdx >= 0 && lightIdx < numLights) {
                    buttonVec[lightIdx] = 1;
                }
            }
            buttons.add(buttonVec);
        }

        int numButtons = buttons.size();
        if (numButtons == 0) return 0;

        // 3. Construct an augmented matrix for the GF(2) system: Matrix A | vector b
        int[][] matrix = new int[numLights][numButtons + 1];
        for (int j = 0; j < numButtons; j++) {
            for (int i = 0; i < numLights; i++) {
                matrix[i][j] = buttons.get(j)[i];
            }
        }
        for (int i = 0; i < numLights; i++) {
            matrix[i][numButtons] = targetVec[i];
        }

        // 4. Transform to Reduced Row Echelon Form (RREF) using Gaussian Elimination
        int pivotRow = 0;
        int[] pivotColAtRow = new int[numLights];
        Arrays.fill(pivotColAtRow, -1);
        
        for (int col = 0; col < numButtons && pivotRow < numLights; col++) {
            int sel = pivotRow;
            while (sel < numLights && matrix[sel][col] == 0) sel++;
            if (sel == numLights) continue; // Column is linearly dependent (free variable)

            // Swap current row with pivot row
            int[] temp = matrix[sel];
            matrix[sel] = matrix[pivotRow];
            matrix[pivotRow] = temp;
            pivotColAtRow[pivotRow] = col;

            // Eliminate entries in this column for all other rows
            for (int i = 0; i < numLights; i++) {
                if (i != pivotRow && matrix[i][col] == 1) {
                    for (int j = col; j <= numButtons; j++) {
                        matrix[i][j] ^= matrix[pivotRow][j];
                    }
                }
            }
            pivotRow++;
        }

        // Check if the system is consistent (no [0 0 ... 0 | 1] rows)
        for (int i = pivotRow; i < numLights; i++) {
            if (matrix[i][numButtons] == 1) return 0;
        }

        // 5. Solve the system and minimize the number of 1s (presses)
        boolean[] isPivot = new boolean[numButtons];
        for (int i = 0; i < pivotRow; i++) {
            isPivot[pivotColAtRow[i]] = true;
        }
        
        List<Integer> freeVars = new ArrayList<>();
        for (int j = 0; j < numButtons; j++) {
            if (!isPivot[j]) freeVars.add(j);
        }

        int minWeight = Integer.MAX_VALUE;
        int numFree = freeVars.size();
        
        // Iterate through all 2^numFree possible values for the free variables
        for (long i = 0; i < (1L << numFree); i++) {
            int currentWeight = 0;
            int[] x = new int[numButtons];
            
            // Set values for free variables based on bits of i
            for (int j = 0; j < numFree; j++) {
                if (((i >> j) & 1L) == 1L) {
                    x[freeVars.get(j)] = 1;
                    currentWeight++;
                }
            }
            
            // Back-substitute to find the unique values for pivot variables
            for (int r = 0; r < pivotRow; r++) {
                int pCol = pivotColAtRow[r];
                int val = matrix[r][numButtons];
                for (int fIdx : freeVars) {
                    if (matrix[r][fIdx] == 1 && x[fIdx] == 1) {
                        val ^= 1;
                    }
                }
                if (val == 1) currentWeight++;
            }
            
            if (currentWeight < minWeight) {
                minWeight = currentWeight;
            }
        }

        return (minWeight == Integer.MAX_VALUE) ? 0 : (long) minWeight;
    }
}

