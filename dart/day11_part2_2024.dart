
import 'dart:io';
import 'dart:convert';

String trimLeadingZeros(String s) {
  int i = 0;
  while (i < s.length - 1 && s[i] == '0') {
    i++;
  }
  return s.substring(i);
}

List<String> splitStone(String s) {
  int mid = s.length ~/ 2;
  String left = trimLeadingZeros(s.substring(0, mid));
  String right = trimLeadingZeros(s.substring(mid));
  if (left.isEmpty) left = '0';
  if (right.isEmpty) right = '0';
  return [left, right];
}

String multiplyBy2024(String s) {
  List<int> num = s.runes.toList();
  List<int> multiplier = '2024'.runes.toList();
  List<int> result = List<int>.filled(num.length + multiplier.length, 0);

  for (int i = num.length - 1; i >= 0; i--) {
    int carry = 0;
    for (int j = multiplier.length - 1; j >= 0; j--) {
      int product = (num[i] - '0'.codeUnitAt(0)) * (multiplier[j] - '0'.codeUnitAt(0)) + result[i + j + 1] + carry;
      result[i + j + 1] = product % 10;
      carry = product ~/ 10;
    }
    result[i] += carry;
  }

  int start = 0;
  while (start < result.length - 1 && result[start] == 0) {
    start++;
  }

  return String.fromCharCodes(result.sublist(start).map((e) => e + '0'.codeUnitAt(0)));
}

void main() {
  File file = File('input.txt');
  String line = file.readAsStringSync().trim();
  List<String> stonesStr = line.split(RegExp(r'\s+'));

  Map<String, int> stonesMap = {};
  for (String s in stonesStr) {
    stonesMap[s] = (stonesMap[s] ?? 0) + 1;
  }

  const int steps = 75;
  for (int step = 0; step < steps; step++) {
    Map<String, int> newStonesMap = {};
    stonesMap.forEach((stone, count) {
      if (stone == '0') {
        newStonesMap['1'] = (newStonesMap['1'] ?? 0) + count;
      } else if (stone.length % 2 == 0) {
        List<String> split = splitStone(stone);
        newStonesMap[split[0]] = (newStonesMap[split[0]] ?? 0) + count;
        newStonesMap[split[1]] = (newStonesMap[split[1]] ?? 0) + count;
      } else {
        String newStone = multiplyBy2024(stone);
        newStonesMap[newStone] = (newStonesMap[newStone] ?? 0) + count;
      }
    });
    stonesMap = newStonesMap;
  }

  int totalStones = stonesMap.values.reduce((a, b) => a + b);
  print(totalStones);
}
