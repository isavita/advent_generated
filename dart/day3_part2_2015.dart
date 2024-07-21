
import 'dart:io';

void main() async {
  // Read input from the file
  final input = await File('input.txt').readAsString();
  
  // Calculate the number of unique houses that receive at least one present
  int uniqueHouses = calculateUniqueHouses(input.trim());
  
  // Print the result
  print(uniqueHouses);
}

int calculateUniqueHouses(String directions) {
  // Use a Set to keep track of unique houses
  Set<String> visitedHouses = {};
  
  // Starting position for Santa and Robo-Santa
  int santaX = 0, santaY = 0;
  int roboX = 0, roboY = 0;
  
  // Mark the starting position as visited
  visitedHouses.add('$santaX,$santaY');
  
  // Iterate through the directions, alternating between Santa and Robo-Santa
  for (int i = 0; i < directions.length; i++) {
    String move = directions[i];
    
    // Determine who is moving
    if (i % 2 == 0) { // Santa's turn
      if (move == '^') {
        santaY++;
      } else if (move == 'v') {
        santaY--;
      } else if (move == '>') {
        santaX++;
      } else if (move == '<') {
        santaX--;
      }
      // Add Santa's new position to the set
      visitedHouses.add('$santaX,$santaY');
    } else { // Robo-Santa's turn
      if (move == '^') {
        roboY++;
      } else if (move == 'v') {
        roboY--;
      } else if (move == '>') {
        roboX++;
      } else if (move == '<') {
        roboX--;
      }
      // Add Robo-Santa's new position to the set
      visitedHouses.add('$roboX,$roboY');
    }
  }
  
  // The number of unique houses visited is the size of the set
  return visitedHouses.length;
}
