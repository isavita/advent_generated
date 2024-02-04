import 'dart:io';

void main() {
  var orbits = {};
  var totalOrbits = 0;

  var lines = File('input.txt').readAsLinesSync();
  for (var line in lines) {
    var parts = line.split(')');
    orbits[parts[1]] = parts[0];
  }

  for (var key in orbits.keys) {
    var obj = key;
    while (orbits.containsKey(obj)) {
      obj = orbits[obj];
      totalOrbits++;
    }
  }

  print(totalOrbits);

  var youPath = [];
  var sanPath = [];

  var obj = 'YOU';
  while (orbits.containsKey(obj)) {
    obj = orbits[obj];
    youPath.add(obj);
  }

  obj = 'SAN';
  while (orbits.containsKey(obj)) {
    obj = orbits[obj];
    sanPath.add(obj);
  }

  var commonAncestor = youPath.firstWhere((element) => sanPath.contains(element));
  var transfers = youPath.indexOf(commonAncestor) + sanPath.indexOf(commonAncestor);

  print(transfers);
}