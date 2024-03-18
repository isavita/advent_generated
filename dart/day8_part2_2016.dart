import 'dart:io';
import 'dart:math';

void main() {
  List<List<bool>> screen = List.generate(6, (_) => List.filled(50, false));

  File input = File('input.txt');
  List<String> instructions = input.readAsLinesSync();

  for (String instruction in instructions) {
    processInstruction(instruction, screen);
  }

  displayScreen(screen);
}

void displayScreen(List<List<bool>> screen) {
  for (List<bool> row in screen) {
    for (bool pixel in row) {
      stdout.write(pixel ? '#' : '.');
    }
    stdout.writeln();
  }
}

void processInstruction(String instruction, List<List<bool>> screen) {
  RegExp rectRegex = RegExp(r'rect (\d+)x(\d+)');
  RegExp rotateRowRegex = RegExp(r'rotate row y=(\d+) by (\d+)');
  RegExp rotateColumnRegex = RegExp(r'rotate column x=(\d+) by (\d+)');

  if (rectRegex.hasMatch(instruction)) {
    List<String> matches = rectRegex.firstMatch(instruction)!.groups([1, 2])!.map((m) => m!).toList();
    int a = int.parse(matches[0]);
    int b = int.parse(matches[1]);
    rect(screen, a, b);
  } else if (rotateRowRegex.hasMatch(instruction)) {
    List<String> matches = rotateRowRegex.firstMatch(instruction)!.groups([1, 2])!.map((m) => m!).toList();
    int a = int.parse(matches[0]);
    int b = int.parse(matches[1]);
    rotateRow(screen, a, b);
  } else if (rotateColumnRegex.hasMatch(instruction)) {
    List<String> matches = rotateColumnRegex.firstMatch(instruction)!.groups([1, 2])!.map((m) => m!).toList();
    int a = int.parse(matches[0]);
    int b = int.parse(matches[1]);
    rotateColumn(screen, a, b);
  }
}

void rect(List<List<bool>> screen, int a, int b) {
  for (int y = 0; y < b; y++) {
    for (int x = 0; x < a; x++) {
      screen[y][x] = true;
    }
  }
}

void rotateRow(List<List<bool>> screen, int row, int shift) {
  List<bool> temp = List.filled(50, false);
  for (int i = 0; i < 50; i++) {
    temp[(i + shift) % 50] = screen[row][i];
  }
  screen[row] = List.from(temp);
}

void rotateColumn(List<List<bool>> screen, int col, int shift) {
  List<bool> temp = List.filled(6, false);
  for (int i = 0; i < 6; i++) {
    temp[(i + shift) % 6] = screen[i][col];
  }
  for (int i = 0; i < 6; i++) {
    screen[i][col] = temp[i];
  }
}