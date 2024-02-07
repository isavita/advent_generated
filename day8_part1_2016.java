
import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Solution {

    private static final int screenWidth = 50;
    private static final int screenHeight = 6;

    public static void main(String[] args) {
        boolean[][] screen = new boolean[screenHeight][screenWidth];

        try {
            File file = new File("input.txt");
            Scanner scanner = new Scanner(file);

            while (scanner.hasNextLine()) {
                processInstruction(scanner.nextLine(), screen);
            }

            System.out.println(countLitPixels(screen));

            scanner.close();
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }

    private static void processInstruction(String instruction, boolean[][] screen) {
        Pattern rectPattern = Pattern.compile("rect (\\d+)x(\\d+)");
        Pattern rotateRowPattern = Pattern.compile("rotate row y=(\\d+) by (\\d+)");
        Pattern rotateColumnPattern = Pattern.compile("rotate column x=(\\d+) by (\\d+)");

        Matcher rectMatcher = rectPattern.matcher(instruction);
        Matcher rotateRowMatcher = rotateRowPattern.matcher(instruction);
        Matcher rotateColumnMatcher = rotateColumnPattern.matcher(instruction);

        if (rectMatcher.find()) {
            int a = Integer.parseInt(rectMatcher.group(1));
            int b = Integer.parseInt(rectMatcher.group(2));
            rect(screen, a, b);
        } else if (rotateRowMatcher.find()) {
            int a = Integer.parseInt(rotateRowMatcher.group(1));
            int b = Integer.parseInt(rotateRowMatcher.group(2));
            rotateRow(screen, a, b);
        } else if (rotateColumnMatcher.find()) {
            int a = Integer.parseInt(rotateColumnMatcher.group(1));
            int b = Integer.parseInt(rotateColumnMatcher.group(2));
            rotateColumn(screen, a, b);
        }
    }

    private static void rect(boolean[][] screen, int a, int b) {
        for (int y = 0; y < b; y++) {
            for (int x = 0; x < a; x++) {
                screen[y][x] = true;
            }
        }
    }

    private static void rotateRow(boolean[][] screen, int row, int shift) {
        boolean[] temp = new boolean[screenWidth];
        for (int i = 0; i < screenWidth; i++) {
            temp[(i + shift) % screenWidth] = screen[row][i];
        }
        screen[row] = temp;
    }

    private static void rotateColumn(boolean[][] screen, int col, int shift) {
        boolean[] temp = new boolean[screenHeight];
        for (int i = 0; i < screenHeight; i++) {
            temp[(i + shift) % screenHeight] = screen[i][col];
        }
        for (int i = 0; i < screenHeight; i++) {
            screen[i][col] = temp[i];
        }
    }

    private static int countLitPixels(boolean[][] screen) {
        int count = 0;
        for (boolean[] row : screen) {
            for (boolean pixel : row) {
                if (pixel) {
                    count++;
                }
            }
        }
        return count;
    }
}
