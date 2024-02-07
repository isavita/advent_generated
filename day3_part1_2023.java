
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class solution {
    public static void main(String[] args) {
        try {
            char[][] matrix = readFileToMatrix("input.txt");
            int sum = sumOfPartNumbers(matrix);
            System.out.println(sum);
        } catch (IOException e) {
            System.out.println("Error reading file: " + e.getMessage());
        }
    }

    public static char[][] readFileToMatrix(String filePath) throws IOException {
        BufferedReader reader = new BufferedReader(new FileReader(filePath));
        String line;
        int numRows = 0;
        while (reader.readLine() != null) {
            numRows++;
        }
        char[][] matrix = new char[numRows][];
        reader.close();

        reader = new BufferedReader(new FileReader(filePath));
        int row = 0;
        while ((line = reader.readLine()) != null) {
            matrix[row] = line.toCharArray();
            row++;
        }
        reader.close();
        return matrix;
    }

    public static int sumOfPartNumbers(char[][] matrix) {
        int sum = 0;
        boolean[][] visited = new boolean[matrix.length][matrix[0].length];

        for (int y = 0; y < matrix.length; y++) {
            for (int x = 0; x < matrix[y].length; x++) {
                if (!visited[y][x] && Character.isDigit(matrix[y][x])) {
                    int[] result = extractNumber(matrix, x, y);
                    int number = result[0];
                    int length = result[1];
                    if (isAdjacentToSymbol(matrix, x, y, length)) {
                        sum += number;
                    }
                    for (int i = 0; i < length; i++) {
                        visited[y][x + i] = true;
                    }
                }
            }
        }

        return sum;
    }

    public static int[] extractNumber(char[][] matrix, int x, int y) {
        StringBuilder numberStr = new StringBuilder();
        while (x < matrix[y].length && Character.isDigit(matrix[y][x])) {
            numberStr.append(matrix[y][x]);
            x++;
        }
        int number = Integer.parseInt(numberStr.toString());
        return new int[]{number, numberStr.length()};
    }

    public static boolean isAdjacentToSymbol(char[][] matrix, int x, int y, int length) {
        for (int i = 0; i < length; i++) {
            if (checkAdjacent(matrix, x + i, y)) {
                return true;
            }
        }
        return false;
    }

    public static boolean checkAdjacent(char[][] matrix, int x, int y) {
        for (int dy = -1; dy <= 1; dy++) {
            for (int dx = -1; dx <= 1; dx++) {
                int adjX = x + dx;
                int adjY = y + dy;
                if (adjY >= 0 && adjY < matrix.length && adjX >= 0 && adjX < matrix[adjY].length) {
                    if (!Character.isDigit(matrix[adjY][adjX]) && matrix[adjY][adjX] != '.') {
                        return true;
                    }
                }
            }
        }
        return false;
    }
}
