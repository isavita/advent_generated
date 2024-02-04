
import 'dart:io';

void main() {
  List<int> basePattern = [0, 1, 0, -1];
  List<int> input = File('input.txt').readAsStringSync().trim().split('').map(int.parse).toList();

  for (int phase = 0; phase < 100; phase++) {
    List<int> output = List<int>.filled(input.length, 0);

    for (int i = 0; i < input.length; i++) {
      for (int j = 0; j < input.length; j++) {
        int patternIndex = ((j + 1) ~/ (i + 1)) % basePattern.length;
        output[i] += input[j] * basePattern[patternIndex];
      }
      output[i] = output[i].abs() % 10;
    }

    input = List.from(output);
  }

  print(input.sublist(0, 8).join());
}
