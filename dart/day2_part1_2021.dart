import 'dart:io';

void main() {
  var inputFile = new File('input.txt');
  List<String> commands = inputFile.readAsLinesSync();

  int horizontalPosition = 0;
  int depth = 0;

  for (var command in commands) {
    var parts = command.split(' ');
    var direction = parts[0];
    var value = int.parse(parts[1]);

    if (direction == 'forward') {
      horizontalPosition += value;
    } else if (direction == 'down') {
      depth += value;
    } else if (direction == 'up') {
      depth -= value;
    }
  }

  print(horizontalPosition * depth);
}