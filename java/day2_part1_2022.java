
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class solution {
    public static void main(String[] args) {
        try {
            BufferedReader reader = new BufferedReader(new FileReader("input.txt"));
            int totalScore = 0;

            String line;
            while ((line = reader.readLine()) != null) {
                char opponent = line.charAt(0);
                char yourMove = line.charAt(2);

                int score = 0;
                if (yourMove == 'X') {
                    score = 1;
                } else if (yourMove == 'Y') {
                    score = 2;
                } else if (yourMove == 'Z') {
                    score = 3;
                }

                if ((opponent == 'A' && yourMove == 'Y') || (opponent == 'B' && yourMove == 'Z') || (opponent == 'C' && yourMove == 'X')) {
                    score += 6;
                } else if (opponent == 'A' && yourMove == 'X' || opponent == 'B' && yourMove == 'Y' || opponent == 'C' && yourMove == 'Z') {
                    score += 3;
                }

                totalScore += score;
            }

            System.out.println(totalScore);
            reader.close();
        } catch (IOException e) {
            System.out.println("Error reading file: " + e.getMessage());
        }
    }
}
