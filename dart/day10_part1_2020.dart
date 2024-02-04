
import 'dart:io';

void main() {
  List<int> adapters = File('input.txt').readAsLinesSync().map(int.parse).toList();
  adapters.sort();

  int oneJoltDiff = 0;
  int threeJoltDiff = 1;
  int currentJoltage = 0;

  for (int adapter in adapters) {
    if (adapter - currentJoltage == 1) {
      oneJoltDiff++;
    } else if (adapter - currentJoltage == 3) {
      threeJoltDiff++;
    }
    currentJoltage = adapter;
  }

  print(oneJoltDiff * threeJoltDiff);
}
