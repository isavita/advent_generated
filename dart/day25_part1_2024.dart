
import 'dart:io';

void main() {
  final input = File('input.txt').readAsStringSync().trim();
  final blocks = input.split('\n\n');

  final locks = <List<int>>[];
  final keys = <List<int>>[];

  for (final block in blocks) {
    final lines = block.split('\n');
    if (lines[0].startsWith('#')) {
      locks.add(parseLock(lines));
    } else if (lines[0].startsWith('.')) {
      keys.add(parseKey(lines));
    }
  }

  var fitCount = 0;
  for (final lock in locks) {
    for (final key in keys) {
      if (fits(lock, key)) {
        fitCount++;
      }
    }
  }

  print(fitCount);
}

List<int> parseLock(List<String> lines) {
  final heights = <int>[];
  final numCols = lines[0].length;
  final numRows = lines.length;

  for (var col = 0; col < numCols; col++) {
    var height = 0;
    for (var row = 1; row < numRows; row++) {
      if (lines[row][col] == '#') {
        height++;
      } else {
        break;
      }
    }
    heights.add(height);
  }
  return heights;
}

List<int> parseKey(List<String> lines) {
    final heights = <int>[];
    final numCols = lines[0].length;
    final numRows = lines.length;
  
    for (var col = 0; col < numCols; col++) {
      var height = 0;
      for (var row = numRows - 2; row >= 0; row--) {
        if (lines[row][col] == '#') {
          height++;
        } else {
          break;
        }
      }
      heights.add(height);
    }
    return heights;
  }


bool fits(List<int> lock, List<int> key) {
  for (var i = 0; i < lock.length; i++) {
    if (lock[i] + key[i] > 5) {
      return false;
    }
  }
  return true;
}
