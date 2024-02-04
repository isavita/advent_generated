import 'dart:io';
import 'dart:convert';

void main() {
  var file = File('input.txt');
  var input = file.readAsStringSync().trim();

  var lengths = input.runes.toList();
  lengths.addAll([17, 31, 73, 47, 23]);

  var list = List<int>.generate(256, (index) => index);
  var currentPosition = 0;
  var skipSize = 0;

  for (var round = 0; round < 64; round++) {
    for (var length in lengths) {
      for (var i = 0; i < length ~/ 2; i++) {
        var start = (currentPosition + i) % 256;
        var end = (currentPosition + length - 1 - i) % 256;
        var temp = list[start];
        list[start] = list[end];
        list[end] = temp;
      }
      currentPosition = (currentPosition + length + skipSize) % 256;
      skipSize++;
    }
  }

  var denseHash = <int>[];
  for (var i = 0; i < 256; i += 16) {
    var xor = 0;
    for (var j = 0; j < 16; j++) {
      xor ^= list[i + j];
    }
    denseHash.add(xor);
  }

  var hexHash = denseHash.map((e) => e.toRadixString(16).padLeft(2, '0')).join();

  print(hexHash);
}