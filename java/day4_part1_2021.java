
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Scanner;

public class Bingo {

    public static void main(String[] args) throws IOException {
        Scanner scanner = new Scanner(new File("input.txt"));

        // Read the drawn numbers
        String[] drawnNumbersStr = scanner.nextLine().split(",");
        int[] drawnNumbers = Arrays.stream(drawnNumbersStr).mapToInt(Integer::parseInt).toArray();

        // Read the bingo boards
        List<int[][]> boards = new ArrayList<>();
        while (scanner.hasNextLine()) {
            scanner.nextLine(); // Skip empty line
            int[][] board = new int[5][5];
            for (int i = 0; i < 5; i++) {
                for (int j = 0; j < 5; j++) {
                    board[i][j] = scanner.nextInt();
                }
            }
            boards.add(board);
        }

        // Play bingo and find the first winning board
        int winningBoardIndex = -1;
        int winningNumber = -1;
        for (int drawnNumber : drawnNumbers) {
            for (int i = 0; i < boards.size(); i++) {
                markNumber(boards.get(i), drawnNumber);
                if (isWinningBoard(boards.get(i))) {
                    winningBoardIndex = i;
                    winningNumber = drawnNumber;
                    break;
                }
            }
            if (winningBoardIndex != -1) {
                break;
            }
        }

        // Calculate the score of the winning board
        int sumOfUnmarkedNumbers = calculateSumOfUnmarkedNumbers(boards.get(winningBoardIndex));
        int finalScore = sumOfUnmarkedNumbers * winningNumber;

        System.out.println(finalScore);

        scanner.close();
    }

    // Marks the given number on the board if it exists
    private static void markNumber(int[][] board, int number) {
        for (int i = 0; i < 5; i++) {
            for (int j = 0; j < 5; j++) {
                if (board[i][j] == number) {
                    board[i][j] = -1; // Mark as -1
                }
            }
        }
    }

    // Checks if the board is a winning board
    private static boolean isWinningBoard(int[][] board) {
        // Check rows
        for (int i = 0; i < 5; i++) {
            boolean rowWin = true;
            for (int j = 0; j < 5; j++) {
                if (board[i][j] != -1) {
                    rowWin = false;
                    break;
                }
            }
            if (rowWin) {
                return true;
            }
        }

        // Check columns
        for (int j = 0; j < 5; j++) {
            boolean colWin = true;
            for (int i = 0; i < 5; i++) {
                if (board[i][j] != -1) {
                    colWin = false;
                    break;
                }
            }
            if (colWin) {
                return true;
            }
        }

        return false;
    }

    // Calculates the sum of unmarked numbers on the board
    private static int calculateSumOfUnmarkedNumbers(int[][] board) {
        int sum = 0;
        for (int i = 0; i < 5; i++) {
            for (int j = 0; j < 5; j++) {
                if (board[i][j] != -1) {
                    sum += board[i][j];
                }
            }
        }
        return sum;
    }
}
