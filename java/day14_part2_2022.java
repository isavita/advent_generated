
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class Main {

    public static void main(String[] args) {
        try {
            BufferedReader reader = new BufferedReader(new FileReader("input.txt"));
            StringBuilder sb = new StringBuilder();
            String line;
            while ((line = reader.readLine()) != null) {
                sb.append(line).append("\n");
            }
            reader.close();
            String input = sb.toString().trim();
            System.out.println(solve(input));
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    static int solve(String input) {
        char[][] matrix = parseInput(input);
        int originCol = 0;
        for (int i = 0; i < matrix[0].length; i++) {
            if (matrix[0][i] == '+') {
                originCol = i;
            }
            matrix[matrix.length - 1][i] = '#';
        }

        int ans = 0;
        while (!dropSand(matrix, originCol)) {
            ans++;
            if (matrix[0][originCol] == 'o') {
                break;
            }
        }
        return ans;
    }

    static char[][] parseInput(String input) {
        List<List<int[]>> coordSets = new ArrayList<>();
        int lowestCol = Integer.MAX_VALUE;
        int highestRow = 0;

        for (String line : input.split("\n")) {
            String[] rawCoords = line.split(" -> ");
            List<int[]> coords = new ArrayList<>();
            for (String rawCoord : rawCoords) {
                String[] rawNums = rawCoord.split(",");
                int col = Integer.parseInt(rawNums[0]);
                int row = Integer.parseInt(rawNums[1]);
                coords.add(new int[]{col, row});
                lowestCol = Math.min(lowestCol, col);
                highestRow = Math.max(highestRow, row);
            }
            coordSets.add(coords);
        }

        int extraLeftSpace = 200;
        int highestCol = 0;
        for (List<int[]> set : coordSets) {
            for (int[] coord : set) {
                coord[0] -= lowestCol - extraLeftSpace;
                highestCol = Math.max(highestCol, coord[0]);
            }
        }

        char[][] matrix = new char[highestRow + 3][highestCol + extraLeftSpace * 2];
        for (char[] row : matrix) {
            Arrays.fill(row, '.');
        }

        for (List<int[]> set : coordSets) {
            for (int i = 1; i < set.size(); i++) {
                int[] prev = set.get(i - 1);
                int[] curr = set.get(i);
                int col1 = prev[0];
                int row1 = prev[1];
                int col2 = curr[0];
                int row2 = curr[1];

                if (col1 == col2) {
                    int start = Math.min(row1, row2);
                    int end = Math.max(row1, row2);
                    for (int r = start; r <= end; r++) {
                        matrix[r][col1] = '#';
                    }
                } else if (row1 == row2) {
                    int start = Math.min(col1, col2);
                    int end = Math.max(col1, col2);
                    for (int c = start; c <= end; c++) {
                        matrix[row1][c] = '#';
                    }
                }
            }
        }

        int originCol = 500 - lowestCol + extraLeftSpace;
        matrix[0][originCol] = '+';
        return matrix;
    }

    static boolean dropSand(char[][] matrix, int originCol) {
        int r = 0;
        int c = originCol;

        while (r < matrix.length - 1) {
            if (matrix[r + 1][c] == '.') {
                r++;
            } else if (matrix[r + 1][c - 1] == '.') {
                r++;
                c--;
            } else if (matrix[r + 1][c + 1] == '.') {
                r++;
                c++;
            } else {
                matrix[r][c] = 'o';
                return false;
            }
        }
        return true;
    }
}
