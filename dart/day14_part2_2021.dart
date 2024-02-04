
import 'dart:io';

void main() {
  var lines = File('input.txt').readAsLinesSync();
  var template = lines[0];
  var rules = <String, String>{};
  for (var i = 1; i < lines.length; i++) {
    var line = lines[i];
    if (line.isEmpty) continue;
    var parts = line.split(' -> ');
    rules[parts[0]] = parts[1];
  }

  var pairCounts = <String, int>{};
  for (var i = 0; i < template.length - 1; i++) {
    pairCounts[template.substring(i, i + 2)] = (pairCounts[template.substring(i, i + 2)] ?? 0) + 1;
  }

  for (var step = 0; step < 40; step++) {
    var newPairCounts = <String, int>{};
    pairCounts.forEach((pair, count) {
      if (rules.containsKey(pair)) {
        var insert = rules[pair]!;
        newPairCounts[pair[0] + insert] = (newPairCounts[pair[0] + insert] ?? 0) + count;
        newPairCounts[insert + pair[1]] = (newPairCounts[insert + pair[1]] ?? 0) + count;
      } else {
        newPairCounts[pair] = (newPairCounts[pair] ?? 0) + count;
      }
    });
    pairCounts = newPairCounts;
  }

  var elementCounts = <String, int>{};
  pairCounts.forEach((pair, count) {
    elementCounts[pair[0]] = (elementCounts[pair[0]] ?? 0) + count;
  });
  elementCounts[template[template.length - 1]] = (elementCounts[template[template.length - 1]] ?? 0) + 1;

  var maxCount = 0, minCount = 9223372036854775807;
  elementCounts.values.forEach((count) {
    if (count > maxCount) maxCount = count;
    if (count < minCount) minCount = count;
  });

  print(maxCount - minCount);
}
