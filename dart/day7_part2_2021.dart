import 'dart:io';

void main() {
  var file = File('input.txt');
  var positions = <int>[];

  file.readAsLinesSync().forEach((line) {
    var numbers = line.split(',');
    numbers.forEach((num_str) {
      var num = int.parse(num_str);
      positions.add(num);
    });
  });

  positions.sort();

  var min_fuel = 9223372036854775807;
  for (var i = positions[0]; i <= positions[positions.length - 1]; i++) {
    var fuel = 0;
    positions.forEach((pos) {
      fuel += calculateNewFuel(pos, i);
    });
    if (fuel < min_fuel) {
      min_fuel = fuel;
    }
  }
  print(min_fuel);
}

int calculateNewFuel(int currentPosition, int newPosition) {
  var diff = (currentPosition - newPosition).abs();
  return (diff * (diff + 1)) ~/ 2;
}