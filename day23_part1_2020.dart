
import 'dart:io';

void main() {
  List<int> cups = File('input.txt').readAsStringSync().trim().runes.map((rune) => int.parse(String.fromCharCode(rune))).toList();
  
  int currentCup = cups[0];
  
  for (int i = 0; i < 100; i++) {
    int currentCupIndex = cups.indexOf(currentCup);
    
    List<int> pickUp = [];
    for (int j = 1; j <= 3; j++) {
      pickUp.add(cups[(currentCupIndex + j) % cups.length]);
    }
    
    for (int cup in pickUp) {
      cups.remove(cup);
    }
    
    int destinationCup = currentCup - 1;
    while (pickUp.contains(destinationCup) || destinationCup < 1) {
      destinationCup = destinationCup > 1 ? destinationCup - 1 : cups.reduce((value, element) => value > element ? value : element);
    }
    
    int destinationCupIndex = cups.indexOf(destinationCup);
    
    for (int j = 1; j <= 3; j++) {
      cups.insert((destinationCupIndex + j), pickUp[j - 1]);
    }
    
    currentCup = cups[(cups.indexOf(currentCup) + 1) % cups.length];
  }
  
  int indexOne = cups.indexOf(1);
  String result = '';
  for (int i = 1; i < cups.length; i++) {
    result += cups[(indexOne + i) % cups.length].toString();
  }
  
  print(result);
}
