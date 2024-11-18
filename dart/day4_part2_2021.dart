
import 'dart:io';
import 'dart:convert';

void main() {
  final input = File('input.txt').readAsStringSync().trim();
  print(solve(input));
}

int solve(String input) {
  final (nums, boards) = parseInput(input);
  
  int lastWinningScore = -1;
  final alreadyWon = <int>{};
  
  for (final n in nums) {
    for (int bi = 0; bi < boards.length; bi++) {
      if (alreadyWon.contains(bi)) continue;
      
      final didWin = boards[bi].pickNum(n);
      if (didWin) {
        lastWinningScore = boards[bi].score() * n;
        alreadyWon.add(bi);
      }
    }
  }
  
  return lastWinningScore;
}

class BoardState {
  final List<List<int>> board;
  final List<List<bool>> picked;
  
  BoardState(this.board) 
    : picked = List.generate(
        board.length, 
        (_) => List.filled(board[0].length, false)
      );
  
  bool pickNum(int num) {
    for (int r = 0; r < board.length; r++) {
      for (int c = 0; c < board[r].length; c++) {
        if (board[r][c] == num) {
          picked[r][c] = true;
        }
      }
    }
    
    for (int i = 0; i < board.length; i++) {
      bool isFullRow = true, isFullCol = true;
      
      for (int j = 0; j < board.length; j++) {
        if (!picked[i][j]) isFullRow = false;
        if (!picked[j][i]) isFullCol = false;
      }
      
      if (isFullRow || isFullCol) return true;
    }
    
    return false;
  }
  
  int score() {
    int total = 0;
    for (int r = 0; r < board.length; r++) {
      for (int c = 0; c < board[r].length; c++) {
        if (!picked[r][c]) total += board[r][c];
      }
    }
    return total;
  }
}

(List<int>, List<BoardState>) parseInput(String input) {
  final parts = input.split('\n\n');
  
  final nums = parts[0].split(',').map(int.parse).toList();
  
  final boards = parts.skip(1).map((grid) {
    return BoardState(
      grid.split('\n').map((line) {
        return line
          .replaceAll('  ', ' ')
          .trim()
          .split(' ')
          .map(int.parse)
          .toList();
      }).toList()
    );
  }).toList();
  
  return (nums, boards);
}
