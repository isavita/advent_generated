
import 'dart:io';

void main() async {
  // Read input from the file
  final input = await File('input.txt').readAsLines();
  
  // Initialize positions
  int headX = 0, headY = 0; // Head position
  int tailX = 0, tailY = 0; // Tail position
  Set<String> visitedPositions = { '0,0' }; // Set to track visited positions

  // Process each movement
  for (var line in input) {
    final parts = line.split(' ');
    final direction = parts[0];
    final steps = int.parse(parts[1]);

    for (int step = 0; step < steps; step++) {
      // Move the head
      switch (direction) {
        case 'R':
          headX++;
          break;
        case 'L':
          headX--;
          break;
        case 'U':
          headY++;
          break;
        case 'D':
          headY--;
          break;
      }

      // Update the tail position if necessary
      if ((headX - tailX).abs() > 1 || (headY - tailY).abs() > 1) {
        if (headX != tailX) {
          tailX += (headX > tailX) ? 1 : -1;
        }
        if (headY != tailY) {
          tailY += (headY > tailY) ? 1 : -1;
        }
      }

      // Add the tail's current position to the visited set
      visitedPositions.add('${tailX},${tailY}');
    }
  }

  // Output the number of unique positions visited by the tail
  print(visitedPositions.length);
}
