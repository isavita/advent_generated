
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class solution {
    private static final int totalRows = 40;

    public static void main(String[] args) {
        String firstRow = readFirstRow("input.txt");
        int safeTilesCount = countSafeTiles(firstRow, totalRows);
        System.out.println(safeTilesCount);
    }

    public static String readFirstRow(String filename) {
        try {
            BufferedReader reader = new BufferedReader(new FileReader(filename));
            return reader.readLine();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    public static int countSafeTiles(String firstRow, int totalRows) {
        String currentRow = firstRow;
        int safeCount = countChar(currentRow, '.');

        for (int i = 1; i < totalRows; i++) {
            StringBuilder nextRow = new StringBuilder();
            for (int j = 0; j < currentRow.length(); j++) {
                if (isTrap(j - 1, j, j + 1, currentRow)) {
                    nextRow.append("^");
                } else {
                    nextRow.append(".");
                    safeCount++;
                }
            }
            currentRow = nextRow.toString();
        }
        return safeCount;
    }

    public static boolean isTrap(int left, int center, int right, String row) {
        char l = safeIfOutOfBounds(left, row);
        char c = row.charAt(center);
        char r = safeIfOutOfBounds(right, row);

        return (l == '^' && c == '^' && r == '.') ||
                (c == '^' && r == '^' && l == '.') ||
                (l == '^' && c == '.' && r == '.') ||
                (r == '^' && c == '.' && l == '.');
    }

    public static char safeIfOutOfBounds(int index, String row) {
        if (index < 0 || index >= row.length()) {
            return '.';
        }
        return row.charAt(index);
    }

    public static int countChar(String str, char character) {
        int count = 0;
        for (int i = 0; i < str.length(); i++) {
            if (str.charAt(i) == character) {
                count++;
            }
        }
        return count;
    }
}
