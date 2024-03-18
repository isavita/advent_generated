import 'dart:io';
import 'dart:math';

class Scrambler {
  List<String> pw;

  Scrambler(this.pw);

  @override
  String toString() => pw.join();

  void swapPositions(int x, int y) {
    String temp = pw[x];
    pw[x] = pw[y];
    pw[y] = temp;
  }

  void swapLetters(String x, String y) {
    int xIndex = pw.indexOf(x);
    int yIndex = pw.indexOf(y);
    swapPositions(xIndex, yIndex);
  }

  void rotate(int steps) {
    int length = pw.length;
    steps = steps % length;
    if (steps < 0) {
      steps += length;
    }
    pw = pw.sublist(length - steps) + pw.sublist(0, length - steps);
  }

  void rotateLetter(String x) {
    int index = pw.indexOf(x);
    if (index >= 4) {
      index++;
    }
    rotate(index + 1);
  }

  void derotateLetter(String x) {
    int index = pw.indexOf(x);
    int rot;
    if (index % 2 == 1) {
      rot = -(index + 1) ~/ 2;
    } else if (index != 0) {
      rot = (6 - index) ~/ 2;
    } else {
      rot = -1;
    }
    rotate(rot);
  }

  void reverse(int x, int y) {
    while (x < y) {
      String temp = pw[x];
      pw[x] = pw[y];
      pw[y] = temp;
      x++;
      y--;
    }
  }

  void move(int x, int y) {
    String ch = pw[x];
    if (x < y) {
      pw.removeAt(x);
      pw.insert(y, ch);
    } else {
      pw.removeAt(x);
      pw.insert(y, ch);
    }
  }

  void scramble(List<String> instructions, int direction) {
    if (direction < 0) {
      instructions = instructions.reversed.toList();
    }
    for (String instruction in instructions) {
      List<String> line = instruction.split(' ');
      switch (line[0]) {
        case 'swap':
          if (line[1] == 'position') {
            int x = int.parse(line[2]);
            int y = int.parse(line[line.length - 1]);
            swapPositions(x, y);
          } else {
            String x = line[2];
            String y = line[line.length - 1];
            swapLetters(x, y);
          }
          break;
        case 'rotate':
          if (line[1] == 'based') {
            if (direction > 0) {
              rotateLetter(line[line.length - 1]);
            } else {
              derotateLetter(line[line.length - 1]);
            }
          } else {
            int x = int.parse(line[2]);
            if (line[1] == 'left') {
              x = -x;
            }
            if (direction < 0) {
              x = -x;
            }
            rotate(x);
          }
          break;
        case 'reverse':
          int x = int.parse(line[2]);
          int y = int.parse(line[line.length - 1]);
          reverse(x, y);
          break;
        case 'move':
          int x = int.parse(line[2]);
          int y = int.parse(line[line.length - 1]);
          if (direction < 0) {
            int temp = x;
            x = y;
            y = temp;
          }
          move(x, y);
          break;
      }
    }
  }

  void unscramble(List<String> instructions) {
    scramble(instructions, -1);
  }
}

void main() async {
  List<String> instructions = await File('input.txt').readAsLines();
  String hashed = 'fbgdceah';
  Scrambler scrambler = Scrambler(hashed.split(''));
  scrambler.unscramble(instructions);
  print(scrambler.toString());
}