
import 'dart:io';

void main() async {
  // Read input from the file
  final input = await File('input.txt').readAsString();
  final instructions = input.split(', ');

  // Part 1: Calculate the final destination
  final finalPosition = calculateFinalPosition(instructions);
  final distance = finalPosition[0].abs() + finalPosition[1].abs();
  print('Part 1: Distance to Easter Bunny HQ is $distance blocks.');

  // Part 2: Find the first location visited twice
  final firstVisitedTwiceDistance = findFirstVisitedTwice(instructions);
  print('Part 2: Distance to the first location visited twice is $firstVisitedTwiceDistance blocks.');
}

List<int> calculateFinalPosition(List<String> instructions) {
  int x = 0, y = 0; // Starting coordinates
  int direction = 0; // 0 = North, 1 = East, 2 = South, 3 = West

  for (var instruction in instructions) {
    final turn = instruction[0];
    final distance = int.parse(instruction.substring(1));

    // Update direction
    direction = (direction + (turn == 'R' ? 1 : -1)) % 4;

    // Move in the current direction
    switch (direction) {
      case 0: // North
        y += distance;
        break;
      case 1: // East
        x += distance;
        break;
      case 2: // South
        y -= distance;
        break;
      case 3: // West
        x -= distance;
        break;
    }
  }

  return [x, y];
}

int findFirstVisitedTwice(List<String> instructions) {
  int x = 0, y = 0; // Starting coordinates
  int direction = 0; // 0 = North, 1 = East, 2 = South, 3 = West
  final visited = <String>{};

  visited.add('$x,$y'); // Add starting position

  for (var instruction in instructions) {
    final turn = instruction[0];
    final distance = int.parse(instruction.substring(1));

    // Update direction
    direction = (direction + (turn == 'R' ? 1 : -1)) % 4;

    // Move in the current direction
    for (var i = 0; i < distance; i++) {
      switch (direction) {
        case 0: // North
          y++;
          break;
        case 1: // East
          x++;
          break;
        case 2: // South
          y--;
          break;
        case 3: // West
          x--;
          break;
      }

      // Check if this position has been visited
      if (visited.contains('$x,$y')) {
        return x.abs() + y.abs(); // Return the distance to the first visited twice location
      }
      visited.add('$x,$y');
    }
  }

  return 0; // In case no location is visited twice
}
