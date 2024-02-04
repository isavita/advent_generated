import 'dart:io';

void main() {
  var file = File('input.txt');
  var lines = file.readAsLinesSync();

  var adapters = [0];
  for (var line in lines) {
    adapters.add(int.parse(line));
  }

  adapters.sort();
  adapters.add(adapters.last + 3);

  print(countArrangements(adapters));
}

int countArrangements(List<int> adapters) {
  var ways = {0: 1};

  for (var i = 1; i < adapters.length; i++) {
    var currentJoltage = adapters[i];
    for (var diff in [1, 2, 3]) {
      ways[currentJoltage] = (ways[currentJoltage] ?? 0) + (ways[currentJoltage - diff] ?? 0);
    }
  }

  return ways[adapters.last] ?? 0;
}