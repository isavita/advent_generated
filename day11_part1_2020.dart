
import 'dart:io';

void main() {
  List<String> seatingArea = File('input.txt').readAsLinesSync();
  List<String> newSeatingArea = List.from(seatingArea);

  while (true) {
    bool changed = false;

    for (int i = 0; i < seatingArea.length; i++) {
      for (int j = 0; j < seatingArea[i].length; j++) {
        if (seatingArea[i][j] == 'L' && countOccupiedSeats(seatingArea, i, j) == 0) {
          newSeatingArea[i] = newSeatingArea[i].replaceRange(j, j + 1, '#');
          changed = true;
        } else if (seatingArea[i][j] == '#' && countOccupiedSeats(seatingArea, i, j) >= 4) {
          newSeatingArea[i] = newSeatingArea[i].replaceRange(j, j + 1, 'L');
          changed = true;
        }
      }
    }

    if (!changed) break;

    seatingArea = List.from(newSeatingArea);
  }

  int occupiedSeats = 0;
  for (String row in seatingArea) {
    occupiedSeats += row.split('#').length - 1;
  }

  print(occupiedSeats);
}

int countOccupiedSeats(List<String> seatingArea, int row, int col) {
  int count = 0;
  for (int i = row - 1; i <= row + 1; i++) {
    for (int j = col - 1; j <= col + 1; j++) {
      if (i >= 0 && i < seatingArea.length && j >= 0 && j < seatingArea[i].length && !(i == row && j == col) && seatingArea[i][j] == '#') {
        count++;
      }
    }
  }
  return count;
}
