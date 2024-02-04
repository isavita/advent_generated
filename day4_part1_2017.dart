
import 'dart:io';

void main() {
  List<String> passphrases = File('input.txt').readAsLinesSync();
  int validCount = 0;

  for (String passphrase in passphrases) {
    List<String> words = passphrase.split(' ');
    Set<String> uniqueWords = Set<String>.from(words);

    if (words.length == uniqueWords.length) {
      validCount++;
    }
  }

  print(validCount);
}
