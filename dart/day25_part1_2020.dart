
import 'dart:io';

void main() {
  List<String> lines = File('input.txt').readAsLinesSync();
  int cardPublicKey = int.parse(lines[0]);
  int doorPublicKey = int.parse(lines[1]);

  int cardLoopSize = findLoopSize(7, cardPublicKey);
  int doorLoopSize = findLoopSize(7, doorPublicKey);

  int encryptionKey = transformSubjectNumber(doorPublicKey, cardLoopSize);
  print(encryptionKey);
}

int findLoopSize(int subjectNumber, int publicKey) {
  int value = 1;
  int loopSize = 0;
  while (value != publicKey) {
    value = (value * subjectNumber) % 20201227;
    loopSize++;
  }
  return loopSize;
}

int transformSubjectNumber(int subjectNumber, int loopSize) {
  int value = 1;
  for (int i = 0; i < loopSize; i++) {
    value = (value * subjectNumber) % 20201227;
  }
  return value;
}
