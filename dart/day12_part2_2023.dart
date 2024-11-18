
import 'dart:io';
import 'dart:convert';

class Row {
  String springs;
  List<int> group;
  
  Row(this.springs, this.group);
}

void main() {
  List<String> input = File('input.txt').readAsLinesSync();
  print(solve(input));
}

int solve(List<String> input) {
  List<Row> rows = parseInput(input);
  
  List<Row> unfoldedRows = rows.map((row) => unfoldRow(row, 5)).toList();
  
  return unfoldedRows.fold(0, (sum, row) => sum + countArrangements(row));
}

List<Row> parseInput(List<String> input) {
  return input.map((line) {
    var parts = line.split(' ');
    return Row(
      parts[0], 
      parts[1].split(',').map(int.parse).toList()
    );
  }).toList();
}

Row unfoldRow(Row row, int unfoldingFactor) {
  String newSprings = List.filled(unfoldingFactor, row.springs).join('?');
  List<int> newGroup = List.filled(unfoldingFactor, row.group).expand((x) => x).toList();
  
  return Row(newSprings, newGroup);
}

int countArrangements(Row row) {
  return _countArrangementsRecursive(row, 0, 0, 0, {});
}

int _countArrangementsRecursive(
  Row row, 
  int iSprings, 
  int iGroup, 
  int iContiguousDamaged, 
  Map<String, int> cache
) {
  if (iSprings == row.springs.length) {
    if (iGroup == row.group.length && iContiguousDamaged == 0) return 1;
    if (iGroup == row.group.length - 1 && iContiguousDamaged == row.group[iGroup]) return 1;
    return 0;
  }

  String cacheKey = '$iSprings,$iGroup,$iContiguousDamaged';
  if (cache.containsKey(cacheKey)) {
    return cache[cacheKey]!;
  }

  int res = 0;
  String char = row.springs[iSprings];
  
  if (char == '.' || char == '?') {
    if (iContiguousDamaged == 0) {
      res += _countArrangementsRecursive(row, iSprings + 1, iGroup, iContiguousDamaged, cache);
    } else if (iContiguousDamaged == row.group[iGroup]) {
      res += _countArrangementsRecursive(row, iSprings + 1, iGroup + 1, 0, cache);
    }
  }
  
  if (char == '#' || char == '?') {
    if (iGroup < row.group.length && iContiguousDamaged < row.group[iGroup]) {
      res += _countArrangementsRecursive(row, iSprings + 1, iGroup, iContiguousDamaged + 1, cache);
    }
  }

  cache[cacheKey] = res;
  return res;
}
