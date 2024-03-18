import 'dart:io';
import 'dart:math';

class BingoBoard {
  List<List<int>> numbers = List.generate(5, (_) => List.filled(5, 0));
  List<List<bool>> marked = List.generate(5, (_) => List.filled(5, false));

  void mark(int number) {
    for (int i = 0; i < 5; i++) {
      for (int j = 0; j < 5; j++) {
        if (numbers[i][j] == number) {
          marked[i][j] = true;
        }
      }
    }
  }

  bool hasWon() {
    for (int i = 0; i < 5; i++) {
      if (isRowMarked(marked[i]) || isColumnMarked(i)) {
        return true;
      }
    }
    return false;
  }

  int unmarkedSum() {
    int sum = 0;
    for (int i = 0; i < 5; i++) {
      for (int j = 0; j < 5; j++) {
        if (!marked[i][j]) {
          sum += numbers[i][j];
        }
      }
    }
    return sum;
  }

  bool isRowMarked(List<bool> row) {
    for (bool marked in row) {
      if (!marked) {
        return false;
      }
    }
    return true;
  }

  bool isColumnMarked(int column) {
    for (List<bool> row in marked) {
      if (!row[column]) {
        return false;
      }
    }
    return true;
  }
}

void main() {
  File file = File('input.txt');
  List<String> lines = file.readAsLinesSync();

  List<int> numbers = lines[0].split(',').map(int.parse).toList();
  List<BingoBoard> boards = [];

  for (int i = 2; i < lines.length; i += 6) {
    BingoBoard board = BingoBoard();
    for (int j = 0; j < 5; j++) {
      List<String> row = lines[i + j].split(' ').where((element) => element.isNotEmpty).toList();
      for (int k = 0; k < 5; k++) {
        board.numbers[j][k] = int.parse(row[k]);
      }
    }
    boards.add(board);
  }

  BingoBoard? winningBoard;
  int? winningNumber;

  for (int number in numbers) {
    for (BingoBoard board in boards) {
      board.mark(number);
      if (board.hasWon()) {
        winningBoard = board;
        winningNumber = number;
        break;
      }
    }
    if (winningBoard != null) {
      break;
    }
  }

  print(winningBoard!.unmarkedSum() * winningNumber!);
}