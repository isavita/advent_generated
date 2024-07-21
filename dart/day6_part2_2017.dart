
import 'dart:io';
import 'dart:convert';

void main() async {
  var data = await File('input.txt').readAsString();
  var banks = data.trim().split(RegExp(r'\s+')).map(int.parse).toList();
  
  var seen = <String, int>{};
  int cycles = 0;

  while (true) {
    var state = banks.join(',');
    if (seen.containsKey(state)) {
      print('The size of the loop is ${cycles - seen[state]!}');
      return;
    }
    seen[state] = cycles;

    int maxIndex = banks.indexOf(banks.reduce((a, b) => a > b ? a : b));
    int blocks = banks[maxIndex];
    banks[maxIndex] = 0;

    for (int i = 1; i <= blocks; i++) {
      banks[(maxIndex + i) % banks.length]++;
    }

    cycles++;
  }
}
