
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class solution {

    static class Hand {
        String cards;
        int bid;

        Hand(String cards, int bid) {
            this.cards = cards;
            this.bid = bid;
        }
    }

    static Map<Character, Integer> valueDict = new HashMap<Character, Integer>() {{
        put('J', 1);
        put('2', 2);
        put('3', 3);
        put('4', 4);
        put('5', 5);
        put('6', 6);
        put('7', 7);
        put('8', 8);
        put('9', 9);
        put('T', 10);
        put('Q', 11);
        put('K', 12);
        put('A', 13);
    }};

    public static void main(String[] args) {
        List<Hand> hands = new ArrayList<>();
        try (BufferedReader br = new BufferedReader(new FileReader("input.txt"))) {
            String line;
            while ((line = br.readLine()) != null) {
                if (line.isEmpty()) {
                    continue;
                }
                String[] parts = line.split(" ");
                String cards = parts[0];
                int bid = Integer.parseInt(parts[1]);
                hands.add(new Hand(cards, bid));
            }
        } catch (IOException e) {
            e.printStackTrace();
        }

        List<Hand>[] matches = new List[7];
        for (int i = 0; i < 7; i++) {
            matches[i] = new ArrayList<>();
        }

        for (Hand hand : hands) {
            Map<Character, Integer> count = new HashMap<>();
            for (char c : hand.cards.toCharArray()) {
                count.put(c, count.getOrDefault(c, 0) + 1);
            }

            if (count.containsKey('J') && count.get('J') > 0) {
                int highV = 0;
                char highKey = 'J';
                for (char y : count.keySet()) {
                    if (y != 'J') {
                        if (count.get(y) > highV) {
                            highKey = y;
                            highV = count.get(y);
                        } else if (count.get(y) == highV && valueDict.get(y) > valueDict.get(highKey)) {
                            highKey = y;
                        }
                    }
                }
                if (highKey != 'J') {
                    count.put(highKey, count.get(highKey) + count.get('J'));
                    count.remove('J');
                }
            }

            int value = 1;
            for (int i : count.values()) {
                value *= i;
            }

            switch (value) {
                case 1:
                    matches[6].add(hand);
                    break;
                case 2:
                    matches[5].add(hand);
                    break;
                case 3:
                    matches[3].add(hand);
                    break;
                case 4:
                    if (count.size() == 2) {
                        matches[1].add(hand);
                    } else {
                        matches[4].add(hand);
                    }
                    break;
                case 5:
                    matches[0].add(hand);
                    break;
                case 6:
                    matches[2].add(hand);
                    break;
                default:
                    System.out.println("oh no");
            }
        }

        List<int[]> convertedMatches = new ArrayList<>();
        for (List<Hand> x : matches) {
            List<int[]> temp = new ArrayList<>();
            for (Hand i : x) {
                String y = i.cards.replace("A", "E")
                                    .replace("T", "A")
                                    .replace("J", "1")
                                    .replace("Q", "C")
                                    .replace("K", "D");
                int val = Integer.parseInt(y, 16);
                temp.add(new int[]{val, i.bid});
            }
            temp.sort((a, b) -> Integer.compare(b[0], a[0]));
            convertedMatches.addAll(temp);
        }

        int total = 0;
        for (int x = 0; x < convertedMatches.size(); x++) {
            total += convertedMatches.get(x)[1] * (convertedMatches.size() - x);
        }

        System.out.println(total);
    }
}
