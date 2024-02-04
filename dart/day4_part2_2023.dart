import 'dart:io';
import 'dart:convert';

class Card {
  Map<String, int> winnings;
  Map<String, int> givens;
  int totalCount;

  Card({required this.winnings, required this.givens, required this.totalCount});
}

int getPointsForCard(Card card) {
  int points = 0;
  card.givens.forEach((given, count) {
    if (card.winnings.containsKey(given)) {
      points += count * card.winnings[given]!;
    }
  });
  return points;
}

Card lexLineIntoCard(String line) {
  List<String> cardData = line.split(': ')[1].split(' | ');

  RegExp re = RegExp(r'[0-9]{1,2}');

  Map<String, int> winnings = {};
  re.allMatches(cardData[0]).forEach((match) {
    String point = match.group(0)!;
    winnings[point] = (winnings[point] ?? 0) + 1;
  });

  Map<String, int> givens = {};
  re.allMatches(cardData[1]).forEach((match) {
    String point = match.group(0)!;
    givens[point] = (givens[point] ?? 0) + 1;
  });

  return Card(winnings: winnings, givens: givens, totalCount: 1);
}

void main() {
  File file = File('input.txt');
  String input = file.readAsStringSync().trim();

  List<Card> cards = [];

  input.split('\n').forEach((line) {
    if (line.isNotEmpty) {
      Card card = lexLineIntoCard(line);
      cards.add(card);
    }
  });

  for (int i = 0; i < cards.length; i++) {
    int points = getPointsForCard(cards[i]);

    for (int j = 1; j <= points; j++) {
      if (i + j < cards.length) {
        cards[i + j].totalCount += 1 * cards[i].totalCount;
      }
    }
  }

  int totalCards = cards.fold(0, (total, card) => total + card.totalCount);

  print(totalCards);
}