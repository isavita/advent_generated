import 'dart:io';

void main() {
  List<String> lines = File('input.txt').readAsLinesSync();
  
  List<int> player1 = [];
  List<int> player2 = [];
  
  int i = 1;
  while (lines[i].isNotEmpty) {
    player1.add(int.parse(lines[i]));
    i++;
  }
  
  i += 2;
  
  while (i < lines.length) {
    player2.add(int.parse(lines[i]));
    i++;
  }
  
  while (player1.isNotEmpty && player2.isNotEmpty) {
    int card1 = player1.removeAt(0);
    int card2 = player2.removeAt(0);
    
    if (card1 > card2) {
      player1.addAll([card1, card2]);
    } else {
      player2.addAll([card2, card1]);
    }
  }
  
  List<int> winningDeck = player1.isNotEmpty ? player1 : player2;
  
  int score = 0;
  for (int i = 0; i < winningDeck.length; i++) {
    score += winningDeck[i] * (winningDeck.length - i);
  }
  
  print(score);
}