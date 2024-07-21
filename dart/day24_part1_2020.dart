
import 'dart:io';

void main() async {
  // Read the input from the file
  final input = await File('input.txt').readAsLines();
  
  // A set to keep track of black tiles
  final Set<String> blackTiles = {};

  // Define the direction movements
  final Map<String, List<int>> directions = {
    'e': [1, 0],
    'se': [0, 1],
    'sw': [-1, 1],
    'w': [-1, 0],
    'nw': [0, -1],
    'ne': [1, -1],
  };

  for (var line in input) {
    int x = 0, y = 0; // Starting at the reference tile (0, 0)

    // Process each instruction
    for (int i = 0; i < line.length; i++) {
      String dir = line[i];
      if (dir == 's' || dir == 'n') {
        // If the direction is 's' or 'n', we need to look at the next character
        dir += line[++i];
      }
      // Update the coordinates based on the direction
      x += directions[dir]![0];
      y += directions[dir]![1];
    }

    // Create a unique key for the tile based on its coordinates
    String tileKey = '$x,$y';

    // Flip the tile: if it's in the set, remove it (turning it white), otherwise add it (turning it black)
    if (blackTiles.contains(tileKey)) {
      blackTiles.remove(tileKey);
    } else {
      blackTiles.add(tileKey);
    }
  }

  // The number of black tiles is the size of the set
  print('Number of black tiles: ${blackTiles.length}');
}
