import 'dart:io';

void main() async {
  final file = File('input.txt');
  final lines = await file.readAsLines();

  final leftList = <int>[];
  final rightList = <int>[];

  for (var line in lines) {
    if (line.trim().isEmpty) continue;
    final parts = line.split(RegExp(r'\s+'));
    if (parts.length >= 2) {
      leftList.add(int.parse(parts[0]));
      rightList.add(int.parse(parts[1]));
    }
  }

  final rightCount = <int, int>{};
  for (var num in rightList) {
    rightCount.update(num, (count) => count + 1, ifAbsent: () => 1);
  }

  var similarityScore = 0;
  for (var num in leftList) {
    final count = rightCount[num] ?? 0;
    similarityScore += num * count;
  }

  print('Similarity score: $similarityScore');
}
