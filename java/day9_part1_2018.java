import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class Main {
    static class Marble {
        int value;
        Marble prev;
        Marble next;
    }

    public static void main(String[] args) throws IOException {
        int[] result = readInput("input.txt");
        int players = result[0];
        int lastMarble = result[1];
        System.out.println(playMarbleGame(players, lastMarble));
    }

    static int[] readInput(String filename) throws IOException {
        BufferedReader br = new BufferedReader(new FileReader(filename));
        String line = br.readLine();
        String[] parts = line.split(" ");
        int players = Integer.parseInt(parts[0]);
        int lastMarble = Integer.parseInt(parts[6]);
        return new int[]{players, lastMarble};
    }

    static int playMarbleGame(int players, int lastMarble) {
        int[] scores = new int[players];
        Marble current = new Marble();
        current.value = 0;
        current.next = current;
        current.prev = current;

        for (int marble = 1; marble <= lastMarble; marble++) {
            if (marble % 23 == 0) {
                int player = marble % players;
                for (int i = 0; i < 7; i++) {
                    current = current.prev;
                }
                scores[player] += marble + current.value;
                current.prev.next = current.next;
                current.next.prev = current.prev;
                current = current.next;
            } else {
                current = current.next;
                Marble newMarble = new Marble();
                newMarble.value = marble;
                newMarble.prev = current;
                newMarble.next = current.next;
                current.next.prev = newMarble;
                current.next = newMarble;
                current = newMarble;
            }
        }

        int maxScore = 0;
        for (int score : scores) {
            if (score > maxScore) {
                maxScore = score;
            }
        }
        return maxScore;
    }
}