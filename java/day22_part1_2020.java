
import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

public class solution {
    public static void main(String[] args) {
        List<Integer> player1Deck = new ArrayList<>();
        List<Integer> player2Deck = new ArrayList<>();
        List<Integer> currentDeck = player1Deck;

        try {
            File file = new File("input.txt");
            Scanner scanner = new Scanner(file);

            while (scanner.hasNextLine()) {
                String line = scanner.nextLine();
                if (line.equals("")) {
                    currentDeck = player2Deck;
                    continue;
                }
                if (line.contains("Player")) {
                    continue;
                }
                int card = Integer.parseInt(line);
                currentDeck.add(card);
            }

            while (!player1Deck.isEmpty() && !player2Deck.isEmpty()) {
                int card1 = player1Deck.remove(0);
                int card2 = player2Deck.remove(0);
                if (card1 > card2) {
                    player1Deck.add(card1);
                    player1Deck.add(card2);
                } else {
                    player2Deck.add(card2);
                    player2Deck.add(card1);
                }
            }

            List<Integer> winningDeck;
            int score = 0;
            if (!player1Deck.isEmpty()) {
                winningDeck = player1Deck;
            } else {
                winningDeck = player2Deck;
            }

            for (int i = 0; i < winningDeck.size(); i++) {
                score += winningDeck.get(i) * (winningDeck.size() - i);
            }
            System.out.println(score);

        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }
}
