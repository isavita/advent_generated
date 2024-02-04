import 'dart:io';

void main() {
  var file = File('input.txt');
  var lines = file.readAsLinesSync();

  var polymer = lines[0];
  var rules = Map<String, String>();

  for (var i = 1; i < lines.length; i++) {
    var line = lines[i];
    if (line.isEmpty) {
      continue;
    }
    var parts = line.split(' -> ');
    rules[parts[0]] = parts[1];
  }

  for (var step = 0; step < 10; step++) {
    polymer = applyInsertion(polymer, rules);
  }

  var counts = countElements(polymer);
  var minMaxValues = minMax(counts);

  print(minMaxValues[1] - minMaxValues[0]);
}

String applyInsertion(String polymer, Map<String, String> rules) {
  var newPolymer = StringBuffer();
  for (var i = 0; i < polymer.length - 1; i++) {
    newPolymer.write(polymer[i]);
    if (rules.containsKey(polymer.substring(i, i + 2))) {
      newPolymer.write(rules[polymer.substring(i, i + 2)]);
    }
  }
  newPolymer.write(polymer[polymer.length - 1]);
  return newPolymer.toString();
}

Map<String, int> countElements(String polymer) {
  var counts = Map<String, int>();
  for (var i = 0; i < polymer.length; i++) {
    var c = polymer[i];
    counts[c] = (counts[c] ?? 0) + 1;
  }
  return counts;
}

List<int> minMax(Map<String, int> counts) {
  var min = 2147483647;
  var max = -2147483648;
  counts.forEach((key, value) {
    if (value < min) {
      min = value;
    }
    if (value > max) {
      max = value;
    }
  });
  return [min, max];
}