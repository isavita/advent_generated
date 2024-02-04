
import 'dart:io';

void main() {
  var matrix = readFileToMatrix("input.txt");

  var sum = sumOfPartNumbers(matrix);
  print(sum);
}

List<List<String>> readFileToMatrix(String filePath) {
  var file = File(filePath);
  var lines = file.readAsLinesSync();

  var matrix = lines.map((line) => line.split('')).toList();
  return matrix;
}

int sumOfPartNumbers(List<List<String>> matrix) {
  var sum = 0;
  var visited = List.generate(matrix.length, (index) => List.filled(matrix[index].length, false));

  for (var y = 0; y < matrix.length; y++) {
    for (var x = 0; x < matrix[y].length; x++) {
      if (!visited[y][x] && isDigit(matrix[y][x])) {
        var result = extractNumber(matrix, x, y);
        var number = result[0];
        var length = result[1];

        if (isAdjacentToSymbol(matrix, x, y, length)) {
          sum += number;
        }

        for (var i = 0; i < length; i++) {
          visited[y][x + i] = true;
        }
      }
    }
  }

  return sum;
}

List<int> extractNumber(List<List<String>> matrix, int x, int y) {
  var numberStr = '';
  while (x < matrix[y].length && isDigit(matrix[y][x])) {
    numberStr += matrix[y][x];
    x++;
  }

  var number = int.parse(numberStr);
  return [number, numberStr.length];
}

bool isAdjacentToSymbol(List<List<String>> matrix, int x, int y, int length) {
  for (var i = 0; i < length; i++) {
    if (checkAdjacent(matrix, x + i, y)) {
      return true;
    }
  }
  return false;
}

bool checkAdjacent(List<List<String>> matrix, int x, int y) {
  for (var dy = -1; dy <= 1; dy++) {
    for (var dx = -1; dx <= 1; dx++) {
      var adjX = x + dx;
      var adjY = y + dy;
      if (adjY >= 0 && adjY < matrix.length && adjX >= 0 && adjX < matrix[adjY].length) {
        if (!isDigit(matrix[adjY][adjX]) && matrix[adjY][adjX] != '.') {
          return true;
        }
      }
    }
  }
  return false;
}

bool isDigit(String s) {
  return int.tryParse(s) != null;
}
