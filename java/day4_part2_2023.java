
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class CardGame {

  static class Card {

    HashMap<String, Integer> winnings;
    HashMap<String, Integer> givens;
    int totalCount;

    Card(HashMap<String, Integer> winnings, HashMap<String, Integer> givens) {
      this.winnings = winnings;
      this.givens = givens;
      this.totalCount = 1;
    }
  }

  static int getPointsForCard(Card card) {
    int points = 0;
    for (String given : card.givens.keySet()) {
      if (card.winnings.containsKey(given)) {
        points++;
      }
    }
    return points;
  }

  static Card lexLineIntoCard(String line) {
    String cardDataStr = line.split(": ")[1];
    String[] cardData = cardDataStr.split(" \\| ");

    HashMap<String, Integer> winnings = new HashMap<>();
    Matcher m = Pattern.compile("[0-9]{1,2}").matcher(cardData[0]);
    while (m.find()) {
      String point = m.group();
      winnings.put(point, winnings.getOrDefault(point, 0) + 1);
    }

    HashMap<String, Integer> givens = new HashMap<>();
    m = Pattern.compile("[0-9]{1,2}").matcher(cardData[1]);
    while (m.find()) {
      String point = m.group();
      givens.put(point, givens.getOrDefault(point, 0) + 1);
    }

    return new Card(winnings, givens);
  }

  public static void main(String[] args) throws IOException {
    List<String> lines = Files.readAllLines(Paths.get("input.txt"));
    Card[] cards = new Card[lines.size()];
    int cardIndex = 0;

    for (String line : lines) {
      if (line.isEmpty()) {
        continue;
      }
      cards[cardIndex++] = lexLineIntoCard(line);
    }

    for (int i = 0; i < cards.length; i++) {
      int points = getPointsForCard(cards[i]);
      for (int j = 1; j <= points && i + j < cards.length; j++) {
        cards[i + j].totalCount += cards[i].totalCount;
      }
    }

    int totalCards = 0;
    for (Card card : cards) {
      totalCards += card.totalCount;
    }
    System.out.println(totalCards);
  }
}
