
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.*;

public class Main {
    static class Hand {
        String cards;
        int bid;

        public Hand(String cards, int bid) {
            this.cards = cards;
            this.bid = bid;
        }
    }

    static class RankedHand {
        Hand hand;
        int rank;

        public RankedHand(Hand hand, int rank) {
            this.hand = hand;
            this.rank = rank;
        }
    }

    static List<List<Hand>> matches = new ArrayList<>();

    public static void findMatches(List<Hand> hands) {
        for (Hand hand : hands) {
            Map<Character, Integer> count = new HashMap<>();
            for (char card : hand.cards.toCharArray()) {
                count.put(card, count.getOrDefault(card, 0) + 1);
            }
            int value = 1;
            for (int c : count.values()) {
                value *= c;
            }
            if (value == 1) {
                matches.get(6).add(hand);
            } else if (value == 2) {
                matches.get(5).add(hand);
            } else if (value == 3) {
                matches.get(3).add(hand);
            } else if (value == 4) {
                if (count.size() == 2) {
                    matches.get(1).add(hand);
                } else {
                    matches.get(4).add(hand);
                }
            } else if (value == 5) {
                matches.get(0).add(hand);
            } else if (value == 6) {
                matches.get(2).add(hand);
            }
        }
    }

    static List<RankedHand> convertAndOrderMatches() {
        List<RankedHand> convertedMatches = new ArrayList<>();
        for (List<Hand> category : matches) {
            List<RankedHand> temp = new ArrayList<>();
            for (Hand hand : category) {
                String cards = hand.cards.replace('A', 'E')
                        .replace('T', 'A')
                        .replace('J', 'B')
                        .replace('Q', 'C')
                        .replace('K', 'D');
                int num = Integer.parseInt(cards, 16);
                temp.add(new RankedHand(hand, num));
            }
            temp.sort((a, b) -> Integer.compare(b.rank, a.rank));
            convertedMatches.addAll(temp);
        }
        return convertedMatches;
    }

    public static void main(String[] args) throws IOException {
        for (int i = 0; i < 7; i++) {
            matches.add(new ArrayList<>());
        }

        BufferedReader reader = new BufferedReader(new FileReader("input.txt"));
        String line;
        List<Hand> hands = new ArrayList<>();
        while ((line = reader.readLine()) != null && !line.isEmpty()) {
            String[] parts = line.split(" ");
            hands.add(new Hand(parts[0], Integer.parseInt(parts[1])));
        }
        reader.close();

        findMatches(hands);
        List<RankedHand> convertedMatches = convertAndOrderMatches();

        long total = 0;
        for (int i = 0; i < convertedMatches.size(); i++) {
            total += (long) convertedMatches.get(i).hand.bid * (convertedMatches.size() - i);
        }
        System.out.println(total);
    }
}
