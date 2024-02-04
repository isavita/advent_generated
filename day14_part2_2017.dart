
import 'dart:io';

void reverseSection(List<int> arr, int start, int length) {
  int n = arr.length;
  for (int i = start, j = start + length - 1; i < j; i++, j--) {
    arr[i % n] ^= arr[j % n];
    arr[j % n] ^= arr[i % n];
    arr[i % n] ^= arr[j % n];
  }
}

String knotHash(String input) {
  List<int> lengths = input.runes.toList();
  lengths.addAll([17, 31, 73, 47, 23]);

  List<int> list = List<int>.generate(256, (index) => index);

  int position = 0;
  int skip = 0;
  for (int round = 0; round < 64; round++) {
    for (int length in lengths) {
      reverseSection(list, position, length);
      position += length + skip;
      skip++;
    }
  }

  List<int> denseHash = List<int>.generate(16, (index) {
    int xor = 0;
    for (int j = 0; j < 16; j++) {
      xor ^= list[index * 16 + j];
    }
    return xor;
  });

  List<int> hexHash = denseHash.map((v) => v).toList();
  return hexHash.map((v) => v.toRadixString(16).padLeft(2, '0')).join();
}

String hexToBinary(String hexStr) {
  String binaryStr = '';
  for (int i = 0; i < hexStr.length; i++) {
    int val = int.parse(hexStr[i], radix: 16);
    binaryStr += val.toRadixString(2).padLeft(4, '0');
  }
  return binaryStr;
}

void dfs(int x, int y, List<List<int>> grid) {
  if (x < 0 || x >= 128 || y < 0 || y >= 128 || grid[x][y] != 1) {
    return;
  }
  grid[x][y] = 0;
  dfs(x - 1, y, grid);
  dfs(x + 1, y, grid);
  dfs(x, y - 1, grid);
  dfs(x, y + 1, grid);
}

void main() {
  File file = File('input.txt');
  String keyString = file.readAsStringSync().trim();
  List<List<int>> grid = List.generate(128, (index) => List<int>.filled(128, 0));
  int totalUsed = 0;
  int regions = 0;

  for (int i = 0; i < 128; i++) {
    String rowKey = '$keyString-$i';
    String hash = knotHash(rowKey);
    String binaryRow = hexToBinary(hash);

    for (int j = 0; j < binaryRow.length; j++) {
      if (binaryRow[j] == '1') {
        grid[i][j] = 1;
        totalUsed++;
      }
    }
  }

  for (int i = 0; i < 128; i++) {
    for (int j = 0; j < 128; j++) {
      if (grid[i][j] == 1) {
        regions++;
        dfs(i, j, grid);
      }
    }
  }

  print(regions);
}
