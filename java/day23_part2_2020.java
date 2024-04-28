import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class Main {
    private static final int totalCups = 1000000;
    private static final int totalMoves = 10000000;

    public static void main(String[] args) throws IOException {
        BufferedReader br = new BufferedReader(new FileReader("input.txt"));
        String input = br.readLine();
        br.close();

        int[] cups = new int[totalCups + 1];
        int lastCup = 0;

        for (char c : input.toCharArray()) {
            cups[lastCup] = c - '0';
            lastCup = c - '0';
        }

        for (int i = input.length() + 1; i <= totalCups; i++) {
            cups[lastCup] = i;
            lastCup = i;
        }
        cups[lastCup] = input.charAt(0) - '0';

        int currentCup = input.charAt(0) - '0';
        for (int i = 0; i < totalMoves; i++) {
            int pickup1 = cups[currentCup];
            int pickup2 = cups[pickup1];
            int pickup3 = cups[pickup2];

            cups[currentCup] = cups[pickup3];

            int destinationCup = currentCup - 1;
            if (destinationCup == 0) {
                destinationCup = totalCups;
            }
            while (destinationCup == pickup1 || destinationCup == pickup2 || destinationCup == pickup3) {
                destinationCup--;
                if (destinationCup == 0) {
                    destinationCup = totalCups;
                }
            }

            cups[pickup3] = cups[destinationCup];
            cups[destinationCup] = pickup1;

            currentCup = cups[currentCup];
        }

        int cup1 = cups[1];
        int cup2 = cups[cup1];
        System.out.println((long) cup1 * cup2);
    }
}