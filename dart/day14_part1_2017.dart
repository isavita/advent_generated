import 'dart:io';

void reverseSection(List<int> arr, int start, int length) {
  int n = arr.length;
  for (int i = start, j = start + length - 1; i < j; i++, j--) {
    int temp = arr[i % n];
    arr[i % n] = arr[j % n];
    arr[j % n] = temp;
  }
}

String knotHash(String input) {
  List<int> lengths = [];
  input.runes.forEach((char) {
    lengths.add(char);
  });
  lengths.addAll([17, 31, 73, 47, 23]);

  List<int> list = List.generate(256, (index) => index);

  int position = 0;
  int skip = 0;
  for (int round = 0; round < 64; round++) {
    for (int length in lengths) {
      reverseSection(list, position, length);
      position += length + skip;
      skip++;
    }
  }

  List<int> denseHash = List.generate(16, (index) {
    int xor = 0;
    for (int j = 0; j < 16; j++) {
      xor ^= list[index * 16 + j];
    }
    return xor;
  });

  List<int> hexHash = denseHash.map((val) => val).toList();
  return hexHash.map((byte) => byte.toRadixString(16)).join();
}

String hexToBinary(String hexStr) {
  String binaryStr = '';
  hexStr.runes.forEach((hexDigit) {
    int val = int.parse(String.fromCharCode(hexDigit), radix: 16);
    binaryStr += val.toRadixString(2).padLeft(4, '0');
  });
  return binaryStr;
}

void main() {
  File file = File('input.txt');
  String keyString = file.readAsStringSync().trim();
  int totalUsed = 0;

  for (int i = 0; i < 128; i++) {
    String rowKey = '$keyString-$i';
    String hash = knotHash(rowKey);
    String binaryRow = hexToBinary(hash);

    totalUsed += binaryRow.split('').where((bit) => bit == '1').length;
  }

  print(totalUsed);
}