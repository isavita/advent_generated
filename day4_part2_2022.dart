
import 'dart:io';

void main() {
  List<String> lines = File('input.txt').readAsLinesSync();

  int fullyContains = 0;
  int overlaps = 0;

  for (String line in lines) {
    List<List<int>> ranges = line.split(',').map((pair) {
      List<int> range = pair.split('-').map(int.parse).toList();
      range.sort();
      return range;
    }).toList();

    if ((ranges[0][0] <= ranges[1][0]) && (ranges[0][1] >= ranges[1][1])) {
      fullyContains++;
    }

    if ((ranges[0][0] <= ranges[1][0] && ranges[1][0] <= ranges[0][1]) ||
        (ranges[1][0] <= ranges[0][0] && ranges[0][0] <= ranges[1][1])) {
      overlaps++;
    }
  }

  print(fullyContains);
  print(overlaps);
}
