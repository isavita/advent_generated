import java.io.File;
import java.io.FileNotFoundException;
import java.util.HashMap;
import java.util.Map;
import java.util.Scanner;

public class Main {

    public static void main(String[] args) throws FileNotFoundException {
        File file = new File("input.txt");
        Scanner scanner = new Scanner(file);
        String input = scanner.useDelimiter("\\Z").next();
        scanner.close();
        long result = solve(input);
        System.out.println(result);
    }

    public static long solve(String input) {
        int[] positions = parseInput(input);
        long[] wins = play(new int[]{positions[0], positions[1]}, new int[]{0, 0}, 3, true, new HashMap<>());
        return wins[0] > wins[1] ? wins[0] : wins[1];
    }

    public static long[] play(int[] positions, int[] scores, int rollsLeftInTurn, boolean isPlayer1sTurn, Map<String, long[]> memo) {
        String key = positions[0] + "," + positions[1] + "," + scores[0] + "," + scores[1] + "," + rollsLeftInTurn + "," + isPlayer1sTurn;
        if (memo.containsKey(key)) {
            return memo.get(key);
        }

        int playerIndex = isPlayer1sTurn ? 0 : 1;

        int[] scoresCopy = {scores[0], scores[1]};
        if (rollsLeftInTurn == 0) {
            scoresCopy[playerIndex] += positions[playerIndex];
            if (scoresCopy[playerIndex] >= 21) {
                if (playerIndex == 0) {
                    return new long[]{1, 0};
                }
                return new long[]{0, 1};
            }
            isPlayer1sTurn = !isPlayer1sTurn;
            rollsLeftInTurn = 3;
            playerIndex = 1 - playerIndex;
        }

        long wins1 = 0;
        long wins2 = 0;
        for (int roll = 1; roll <= 3; roll++) {
            int[] positionsCopy = {positions[0], positions[1]};
            positionsCopy[playerIndex] += roll;
            if (positionsCopy[playerIndex] > 10) {
                positionsCopy[playerIndex] -= 10;
            }
            long[] res = play(positionsCopy, scoresCopy, rollsLeftInTurn - 1, isPlayer1sTurn, memo);
            wins1 += res[0];
            wins2 += res[1];
        }

        memo.put(key, new long[]{wins1, wins2});
        return new long[]{wins1, wins2};
    }

    public static int[] parseInput(String input) {
        String[] lines = input.split("\n");
        int[] ans = new int[2];
        for (int i = 0; i < 2; i++) {
            String line = lines[i];
            int player, startingPosition;
            String[] parts = line.split(" ");
            player = Integer.parseInt(parts[1].replace(":", ""));
            startingPosition = Integer.parseInt(parts[4].replace(",", ""));
            ans[i] = startingPosition;
        }
        return ans;
    }
}