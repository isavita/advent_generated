
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class solution {
    public static void main(String[] args) {
        try {
            BufferedReader reader = new BufferedReader(new FileReader("input.txt"));
            String line1 = reader.readLine();
            String line2 = reader.readLine();
            reader.close();

            int player1Start = Integer.parseInt(line1.substring(28).trim());
            int player2Start = Integer.parseInt(line2.substring(28).trim());
            int player1Pos = player1Start;
            int player2Pos = player2Start;
            int player1Score = 0;
            int player2Score = 0;
            int dieRoll = 1;
            int rollCount = 0;

            while (true) {
                // Player 1
                int rolls = dieRoll % 100 + (dieRoll + 1) % 100 + (dieRoll + 2) % 100;
                rollCount += 3;
                dieRoll += 3;
                player1Pos = (player1Pos + rolls - 1) % 10 + 1;
                player1Score += player1Pos;
                if (player1Score >= 1000) {
                    System.out.println(player2Score * rollCount);
                    break;
                }

                // Player 2
                rolls = dieRoll % 100 + (dieRoll + 1) % 100 + (dieRoll + 2) % 100;
                rollCount += 3;
                dieRoll += 3;
                player2Pos = (player2Pos + rolls - 1) % 10 + 1;
                player2Score += player2Pos;
                if (player2Score >= 1000) {
                    System.out.println(player1Score * rollCount);
                    break;
                }
            }
        } catch (IOException e) {
            System.out.println("Error reading file: " + e.getMessage());
            System.exit(1);
        }
    }
}
