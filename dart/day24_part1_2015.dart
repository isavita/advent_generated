import 'dart:io';
import 'dart:math';

void main() {
  final lines = File('input.txt').readAsLinesSync();
  final packages = lines.map(int.parse).toList();
  final totalWeight = packages.fold(0, (sum, weight) => sum + weight);
  final targetWeight = totalWeight ~/ 3;
  int bestQE = double.maxFinite.toInt();
  int bestLength = double.maxFinite.toInt();

  for (int comb = 1; comb < (1 << packages.length); comb++) {
    int groupWeight = 0, qe = 1, groupLength = 0;
    for (int i = 0; i < packages.length; i++) {
      if (comb & (1 << i) != 0) {
        groupWeight += packages[i];
        qe *= packages[i];
        groupLength++;
      }
    }
    if (groupWeight == targetWeight && groupLength <= bestLength) {
      if (groupLength < bestLength || qe < bestQE) {
        bestLength = groupLength;
        bestQE = qe;
      }
    }
  }

  print(bestQE);
}