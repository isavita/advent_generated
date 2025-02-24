
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayDeque;
import java.util.Deque;

public class MarbleGame {

    public static void main(String[] args) {
        try (BufferedReader br = new BufferedReader(new FileReader("input.txt"))) {
            String[] data = br.readLine().trim().split(" ");
            int players = Integer.parseInt(data[0]);
            int lastMarble = Integer.parseInt(data[6]) * 100;

            Deque<Integer> circle = new ArrayDeque<>();
            circle.add(0);

            long[] scores = new long[players];
            int currentPlayer = 0;

            for (int marble = 1; marble <= lastMarble; marble++) {
                if (marble % 23 == 0) {
                    for (int i = 0; i < 7; i++) {
                        circle.addFirst(circle.removeLast());
                    }
                    scores[currentPlayer] += marble + circle.removeLast();
                    circle.addLast(circle.removeFirst());
                } else {
                    circle.addLast(circle.removeFirst());
                    circle.addLast(marble);
                }

                currentPlayer = (currentPlayer + 1) % players;
            }

            long maxScore = 0;
            for (long score : scores) {
                if (score > maxScore) {
                    maxScore = score;
                }
            }

            System.out.println(maxScore);

        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
