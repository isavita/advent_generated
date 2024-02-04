
import 'dart:io';

void main() {
  var orbits = {};
  var totalOrbits = 0;

  var lines = File('input.txt').readAsLinesSync();
  for (var line in lines) {
    var objects = line.split(')');
    orbits[objects[1]] = objects[0];
  }

  for (var obj in orbits.keys) {
    var current = obj;
    while (orbits.containsKey(current)) {
      current = orbits[current];
      totalOrbits++;
    }
  }

  print(totalOrbits);
}
