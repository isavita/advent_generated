
import 'dart:io';

List<int> copyDeck(List<int> d, int n) {
  List<int> copy = List<int>.filled(n, 0);
  for (int i = 0; i < n; i++) {
    copy[i] = d[i];
  }
  return copy;
}

int calculateScore(List<int> d) {
  int score = 0;
  for (int i = 0; i < d.length; i++) {
    score += d[i] * (d.length - i);
  }
  return score;
}

List<List<int>> playRecursiveCombat(List<int> player1, List<int> player2) {
  Map<String, bool> previousRounds = {};
  while (player1.length > 0 && player2.length > 0) {
    String roundKey = "${player1}|${player2}";
    if (previousRounds[roundKey] != null) {
      return [player1, []];
    }
    previousRounds[roundKey] = true;

    int card1 = player1[0];
    int card2 = player2[0];
    player1 = player1.sublist(1);
    player2 = player2.sublist(1);

    if (player1.length >= card1 && player2.length >= card2) {
      List<List<int>> subPlayers = playRecursiveCombat(copyDeck(player1, card1), copyDeck(player2, card2));
      if (subPlayers[0].length > 0) {
        player1.addAll([card1, card2]);
      } else {
        player2.addAll([card2, card1]);
      }
    } else {
      if (card1 > card2) {
        player1.addAll([card1, card2]);
      } else {
        player2.addAll([card2, card1]);
      }
    }
  }
  return [player1, player2];
}

void main() {
  File file = File('input.txt');
  List<int> player1Deck = [];
  List<int> player2Deck = [];
  List<int> winningDeck = [];

  List<int> currentDeck = player1Deck;
  String contents = file.readAsStringSync();
  List<String> lines = contents.split('\n');

  for (String line in lines) {
    if (line.isEmpty) {
      currentDeck = player2Deck;
      continue;
    }
    if (line.contains('Player')) {
      continue;
    }
    int card = int.parse(line);
    currentDeck.add(card);
  }

  List<List<int>> result = playRecursiveCombat(player1Deck, player2Deck);
  if (result[0].length > 0) {
    winningDeck = result[0];
  } else {
    winningDeck = result[1];
  }

  print(calculateScore(winningDeck));
}
