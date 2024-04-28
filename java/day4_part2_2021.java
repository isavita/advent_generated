import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.*;

public class Main {
    public static void main(String[] args) throws IOException {
        BufferedReader br = new BufferedReader(new FileReader("input.txt"));
        String input = "";
        String line;
        while ((line = br.readLine()) != null) {
            input += line + "\n";
        }
        br.close();

        int result = solve(input);
        System.out.println(result);
    }

    static int solve(String input) {
        String[] lines = input.trim().split("\n\n");
        String[] numsStr = lines[0].split(",");
        int[] nums = new int[numsStr.length];
        for (int i = 0; i < numsStr.length; i++) {
            nums[i] = Integer.parseInt(numsStr[i].trim());
        }

        List<BoardState> boards = new ArrayList<>();
        for (int i = 1; i < lines.length; i++) {
            String[] gridLines = lines[i].trim().split("\n");
            int[][] board = new int[5][5];
            for (int j = 0; j < 5; j++) {
                String[] parts = gridLines[j].trim().split("\\s+");
                for (int k = 0; k < 5; k++) {
                    board[j][k] = Integer.parseInt(parts[k].trim());
                }
            }
            boards.add(new BoardState(board));
        }

        int lastWinningScore = -1;
        Set<Integer> alreadyWon = new HashSet<>();
        for (int num : nums) {
            for (int bi = 0; bi < boards.size(); bi++) {
                if (alreadyWon.contains(bi)) {
                    continue;
                }
                BoardState b = boards.get(bi);
                if (b.pickNum(num)) {
                    lastWinningScore = b.score() * num;
                    alreadyWon.add(bi);
                }
            }
        }

        return lastWinningScore;
    }

    static class BoardState {
        int[][] board;
        boolean[][] picked;

        BoardState(int[][] board) {
            this.board = board;
            this.picked = new boolean[5][5];
        }

        boolean pickNum(int num) {
            for (int i = 0; i < 5; i++) {
                for (int j = 0; j < 5; j++) {
                    if (board[i][j] == num) {
                        picked[i][j] = true;
                    }
                }
            }

            for (int i = 0; i < 5; i++) {
                boolean isFullRow = true, isFullCol = true;
                for (int j = 0; j < 5; j++) {
                    if (!picked[i][j]) {
                        isFullRow = false;
                    }
                    if (!picked[j][i]) {
                        isFullCol = false;
                    }
                }
                if (isFullRow || isFullCol) {
                    return true;
                }
            }

            return false;
        }

        int score() {
            int score = 0;
            for (int i = 0; i < 5; i++) {
                for (int j = 0; j < 5; j++) {
                    if (!picked[i][j]) {
                        score += board[i][j];
                    }
                }
            }
            return score;
        }
    }
}