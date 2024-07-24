
import java.io.*;
import java.util.*;

public class RecursiveCombat {
    static class Deck {
        List<Integer> cards;

        Deck() {
            cards = new ArrayList<>();
        }

        void add(int card) {
            cards.add(card);
        }

        int size() {
            return cards.size();
        }

        int get(int index) {
            return cards.get(index);
        }

        void removeFirst() {
            cards.remove(0);
        }

        Deck copy(int n) {
            Deck copy = new Deck();
            for (int i = 0; i < n; i++) {
                copy.add(cards.get(i));
            }
            return copy;
        }

        int score() {
            int score = 0;
            for (int i = 0; i < cards.size(); i++) {
                score += cards.get(i) * (cards.size() - i);
            }
            return score;
        }
    }

    static Deck[] playRecursiveCombat(Deck player1, Deck player2) {
        Set<String> previousRounds = new HashSet<>();
        while (player1.size() > 0 && player2.size() > 0) {
            String roundKey = player1.cards.toString() + "|" + player2.cards.toString();
            if (previousRounds.contains(roundKey)) {
                return new Deck[]{player1, new Deck()};
            }
            previousRounds.add(roundKey);

            int card1 = player1.get(0);
            int card2 = player2.get(0);
            player1.removeFirst();
            player2.removeFirst();

            if (player1.size() >= card1 && player2.size() >= card2) {
                Deck subPlayer1 = playRecursiveCombat(player1.copy(card1), player2.copy(card2))[0];
                if (subPlayer1.size() > 0) {
                    player1.add(card1);
                    player1.add(card2);
                } else {
                    player2.add(card2);
                    player2.add(card1);
                }
            } else {
                if (card1 > card2) {
                    player1.add(card1);
                    player1.add(card2);
                } else {
                    player2.add(card2);
                    player2.add(card1);
                }
            }
        }
        return new Deck[]{player1, player2};
    }

    public static void main(String[] args) throws IOException {
        BufferedReader reader = new BufferedReader(new FileReader("input.txt"));
        Deck player1Deck = new Deck();
        Deck player2Deck = new Deck();
        Deck currentDeck = player1Deck;
        String line;

        while ((line = reader.readLine()) != null) {
            if (line.isEmpty()) {
                currentDeck = player2Deck;
                continue;
            }
            if (line.contains("Player")) {
                continue;
            }
            currentDeck.add(Integer.parseInt(line));
        }
        reader.close();

        Deck[] result = playRecursiveCombat(player1Deck, player2Deck);
        Deck winningDeck = result[0].size() > 0 ? result[0] : result[1];
        System.out.println(winningDeck.score());
    }
}
