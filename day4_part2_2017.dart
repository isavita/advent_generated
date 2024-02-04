import 'dart:io';

void main() {
  List<String> passphrases = File('input.txt').readAsLinesSync();
  
  int validPassphrasesPart1 = 0;
  int validPassphrasesPart2 = 0;
  
  for (String passphrase in passphrases) {
    List<String> words = passphrase.split(' ');
    
    if (isValidPassphrasePart1(words)) {
      validPassphrasesPart1++;
    }
    
    if (isValidPassphrasePart2(words)) {
      validPassphrasesPart2++;
    }
  }
  
  print('Part 1: $validPassphrasesPart1');
  print('Part 2: $validPassphrasesPart2');
}

bool isValidPassphrasePart1(List<String> words) {
  return Set<String>.from(words).length == words.length;
}

bool isValidPassphrasePart2(List<String> words) {
  List<String> sortedWords = words.map((word) => String.fromCharCodes(word.runes.toList()..sort())).toList();
  
  return Set<String>.from(sortedWords).length == sortedWords.length;
}