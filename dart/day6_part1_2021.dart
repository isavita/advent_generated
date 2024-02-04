
import 'dart:io';

void main() {
  File file = File('input.txt');
  List<int> fishes = List<int>.filled(9, 0);

  file.readAsLines().then((List<String> lines) {
    List<String> fishStrs = lines[0].split(',');
    fishStrs.forEach((fishStr) {
      int fish = int.parse(fishStr);
      fishes[fish]++;
    });

    for (int day = 1; day <= 80; day++) {
      int newFish = fishes[0];
      for (int i = 1; i < fishes.length; i++) {
        fishes[i - 1] = fishes[i];
      }
      fishes[6] += newFish;
      fishes[8] = newFish;
    }

    int totalFish = fishes.reduce((value, element) => value + element);
    print(totalFish);
  });
}
