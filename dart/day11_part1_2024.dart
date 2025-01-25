
import 'dart:io';
import 'dart:convert';

void main() {
  final inputFile = File('input.txt');
  final stones = inputFile.readAsStringSync().trim().split(' ').map(int.parse).toList();

  List<int> currentStones = stones;

  for (int blink = 0; blink < 25; blink++) {
    List<int> nextStones = [];
    for (int stone in currentStones) {
      if (stone == 0) {
        nextStones.add(1);
      } else {
        String stoneStr = stone.toString();
        if (stoneStr.length % 2 == 0) {
          int mid = stoneStr.length ~/ 2;
          String leftStr = stoneStr.substring(0, mid);
          String rightStr = stoneStr.substring(mid);
          nextStones.add(int.parse(leftStr));
          nextStones.add(int.parse(rightStr));
        } else {
          nextStones.add(stone * 2024);
        }
      }
    }
    currentStones = nextStones;
  }

  stdout.write(currentStones.length);
}
