
import 'dart:io';

void main() {
  final inputFile = File('input.txt');
  final lines = inputFile.readAsLinesSync();

  int totalSum = 0;
  for (final line in lines) {
    int secret = int.parse(line);
    for (int i = 0; i < 2000; i++) {
      secret = _nextSecret(secret);
    }
    totalSum += secret;
  }

  print(totalSum);
}

int _nextSecret(int secret) {
  secret = _mixAndPrune(secret, secret * 64);
  secret = _mixAndPrune(secret, secret ~/ 32);
  secret = _mixAndPrune(secret, secret * 2048);
  return secret;
}

int _mixAndPrune(int secret, int value) {
  return (secret ^ value) % 16777216;
}
